#![feature(rustc_private, decl_macro, plugin, try_blocks, proc_macro_hygiene)]
#![feature(never_type)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]
#![warn(rust_2018_idioms)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

#[macro_use]
extern crate rocket;

mod render;
mod step;
mod watch;

use std::ops::FnOnce;
use std::path::PathBuf;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};

use miri::Evaluator;
use rustc_driver::Compilation;
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_interface::interface;
use rustc_middle::mir;
use rustc_middle::ty::TyCtxt;

use rocket::response::content::*;
use rocket::response::status::BadRequest;
use rocket::response::NamedFile;
use rocket::State;

use serde::Deserialize;
use step::{step, ShouldContinue};

use crate::step::BreakpointTree;

fn should_hide_stmt(stmt: &mir::Statement<'_>) -> bool {
    use rustc_middle::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Nop => true,
        _ => false,
    }
}

type InterpCx<'tcx> = rustc_mir::interpret::InterpCx<'tcx, 'tcx, Evaluator<'tcx, 'tcx>>;

pub struct PrirodaContext<'a, 'tcx: 'a> {
    ecx: InterpCx<'tcx>,
    step_count: &'a mut u128,
    traces: watch::Traces<'tcx>,
    config: &'a mut Config,
    skip_to_main: bool,
}

impl<'a, 'tcx: 'a> PrirodaContext<'a, 'tcx> {
    fn restart(&mut self) {
        self.ecx = create_ecx(self.ecx.tcx.tcx);
        *self.step_count = 0;
        self.traces.clear(); // Cleanup all traces
        self.to_main();
    }

    // Step to main
    fn to_main(&mut self) {
        if self.skip_to_main {
            let main_id = self
                .ecx
                .tcx
                .tcx
                .entry_fn(LOCAL_CRATE)
                .expect("no main or start function found")
                .0
                .to_def_id();

            let _ = step(self, |ecx| {
                let frame = ecx.frame();
                if main_id == frame.instance.def_id() {
                    ShouldContinue::Stop
                } else {
                    ShouldContinue::Continue
                }
            });
        }
    }
}

#[derive(Deserialize)]
pub struct Config {
    #[serde(default = "true_bool")]
    auto_refresh: bool,
    #[serde(default = "default_theme")]
    theme: String,
    #[serde(default)]
    bptree: BreakpointTree,
}

fn true_bool() -> bool {
    true
}
fn default_theme() -> String {
    "default".to_string()
}

impl Default for Config {
    fn default() -> Self {
        ::std::fs::File::open("config.json")
            .map(|f| serde_json::from_reader(f).unwrap())
            .unwrap_or(Config {
                auto_refresh: true,
                theme: "default".to_string(),
                bptree: step::BreakpointTree::default(),
            })
    }
}

type RResult<T> = Result<T, Html<String>>;

fn create_ecx<'mir, 'tcx>(tcx: TyCtxt<'tcx>) -> InterpCx<'tcx> {
    let (main_id, _) = tcx
        .entry_fn(LOCAL_CRATE)
        .expect("no main or start function found");

    miri::create_ecx(
        tcx,
        main_id.to_def_id(),
        miri::MiriConfig {
            args: vec![],
            communicate: true,
            excluded_env_vars: vec![],
            ignore_leaks: true,
            seed: None,
            tracked_pointer_tag: None,
            tracked_alloc_id: None,
            tracked_call_id: None,
            validate: true,
            stacked_borrows: false,
            check_alignment: miri::AlignmentCheck::None,
            track_raw: false,
            data_race_detector: false,
            cmpxchg_weak_failure_rate: 0.8,
        },
    )
    .unwrap()
    .0
}

pub struct PrirodaSender(
    Mutex<::std::sync::mpsc::Sender<Box<dyn FnOnce(&mut PrirodaContext<'_, '_>) + Send>>>,
);

impl PrirodaSender {
    fn do_work<'r, T, F>(&self, f: F) -> Result<T, Html<String>>
    where
        T: rocket::response::Responder<'r> + Send + 'static,
        F: FnOnce(&mut PrirodaContext<'_, '_>) -> T + Send + 'static,
    {
        let (tx, rx) = mpsc::sync_channel(0);
        let sender = self.0.lock().unwrap_or_else(|err| err.into_inner());
        match sender.send(Box::new(move |pcx: &mut PrirodaContext<'_, '_>| {
            tx.send(f(pcx)).unwrap();
        })) {
            Ok(()) => match rx.recv() {
                Ok(val) => Ok(val),
                Err(mpsc::RecvError) => Err(Html(
                    "<center><h1>Miri crashed please go to <a href='/'>index</a></h1></center>"
                        .to_string(),
                )),
            },
            Err(_) => Err(Html(
                "<center><h1>Miri crashed too often. Please restart priroda.</h1></center>"
                    .to_string(),
            )),
        }
    }
}

#[get("/please_panic")]
fn please_panic(sender: State<'_, PrirodaSender>) -> RResult<()> {
    sender.do_work(|_pcx| panic!("You requested a panic"))
}

#[cfg(not(feature = "static_resources"))]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<NamedFile, std::io::Error> {
    let mut res_path = PathBuf::from("./resources/");
    res_path.push(path);
    NamedFile::open(res_path)
}

#[cfg(feature = "static_resources")]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<Content<&'static str>, std::io::Error> {
    use rocket::http::ContentType;
    use std::io::{Error, ErrorKind};
    match path.as_os_str().to_str() {
        Some("svg-pan-zoom.js") => Ok(Content(
            ContentType::JavaScript,
            include_str!("../resources/svg-pan-zoom.js"),
        )),
        Some("zoom_mir.js") => Ok(Content(
            ContentType::JavaScript,
            include_str!("../resources/zoom_mir.js"),
        )),
        Some("style-default.css") => Ok(Content(
            ContentType::CSS,
            include_str!("../resources/style-default.css"),
        )),
        Some("positioning.css") => Ok(Content(
            ContentType::CSS,
            include_str!("../resources/positioning.css"),
        )),
        _ => Err(Error::new(ErrorKind::InvalidInput, "Unknown resource")),
    }
}

#[get("/step_count")]
fn step_count(sender: State<'_, PrirodaSender>) -> RResult<String> {
    sender.do_work(|pcx| format!("{}", pcx.step_count))
}

fn server(sender: PrirodaSender) {
    use rocket::config::Value;
    rocket::ignite()
        .manage(sender)
        .mount("/", routes![please_panic, resources, step_count])
        .mount("/", render::routes::routes())
        .mount("/breakpoints", step::bp_routes::routes())
        .mount("/step", step::step_routes::routes())
        .mount("/watch", watch::routes())
        .attach(rocket::fairing::AdHoc::on_launch(
            "Priroda, because code has no privacy rights",
            |rocket| {
                let config = rocket.config();
                if config.extras.get("spawn_browser") == Some(&Value::Boolean(true)) {
                    let addr = format!("http://{}:{}", config.address, config.port);
                    if open::that(&addr).is_err() {
                        println!("open {} in your browser", addr);
                    }
                }
            },
        ))
        .launch();
}

// Copied from miri/bin/miri.rs
fn find_sysroot() -> String {
    if let Ok(sysroot) = std::env::var("MIRI_SYSROOT") {
        return sysroot;
    }

    // Taken from https://github.com/rust-lang/rust-clippy/pull/911
    let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
    let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => option_env!("RUST_SYSROOT")
            .expect("need to specify RUST_SYSROOT env var or use rustup or multirust")
            .to_owned(),
    }
}

struct PrirodaCompilerCalls {
    step_count: Arc<Mutex<u128>>,
    config: Arc<Mutex<Config>>,
    receiver: Arc<Mutex<mpsc::Receiver<Box<dyn FnOnce(&mut PrirodaContext<'_, '_>) + Send>>>>,
    skip_to_main: bool,
}

impl rustc_driver::Callbacks for PrirodaCompilerCalls {
    fn after_analysis<'tcx>(
        &mut self,
        compiler: &interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            let mut step_count = self
                .step_count
                .lock()
                .unwrap_or_else(|err| err.into_inner());
            let mut config = self.config.lock().unwrap_or_else(|err| err.into_inner());

            let mut pcx = PrirodaContext {
                ecx: create_ecx(tcx),
                step_count: &mut *step_count,
                traces: watch::Traces::new(),
                config: &mut *config,
                skip_to_main: self.skip_to_main,
            };
            // Whether we reset to the same place where miri crashed
            let mut reset = false;
            // Step to the position where miri crashed if it crashed
            for _ in 0..*pcx.step_count {
                reset = true;
                match pcx.ecx.step() {
                    Ok(true) => {}
                    res => panic!("Miri is not deterministic causing error {:?}", res),
                }
            }

            // Just ignore poisoning by panicking
            let receiver = self.receiver.lock().unwrap_or_else(|err| err.into_inner());

            // At the very beginning, go to the start of main unless we have already crashed and are trying to reset
            if !reset {
                pcx.to_main();
            }
            // process commands
            for command in receiver.iter() {
                command(&mut pcx);
            }
        });

        compiler.session().abort_if_errors();

        Compilation::Stop
    }
}

fn main() {
    rustc_driver::init_rustc_env_logger();
    let mut args: Vec<String> = std::env::args().collect();

    // TODO: Move to a 'proper' argument parser
    // Maybe use xflags?

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }

    // Use the --no-main flag to stop
    let stops_at_start = args
        .iter()
        .enumerate()
        .find(|(_, v)| v.as_str() == "--no-main")
        .map(|(i, _)| i);

    if let Some(index) = stops_at_start {
        // We pass these arguments to miri.
        // This is the lowest-effort way to not pass our custom argument to miri
        args.remove(index);
    }
    // Eagerly initialise syntect static
    // Makes highlighting performance profiles clearer
    render::initialise_statics();

    // setup http server and similar
    let (sender, receiver) = mpsc::channel();
    let sender = PrirodaSender(Mutex::new(sender));
    let step_count = Arc::new(Mutex::new(0));
    let config = Arc::new(Mutex::new(Config::default()));

    let handle = std::thread::spawn(move || {
        let args = Arc::new(args);
        let receiver = Arc::new(Mutex::new(receiver));
        for i in 0..5 {
            if i != 0 {
                println!(
                    "\n============== Miri crashed - restart try {} ==============\n",
                    i
                );
            }
            let step_count = step_count.clone();
            let config = config.clone();
            let receiver = receiver.clone();
            let args = args.clone();
            // Ignore result to restart in case of a crash
            let _ = std::thread::spawn(move || {
                let _ = rustc_driver::catch_fatal_errors(move || {
                    rustc_driver::RunCompiler::new(
                        &*args,
                        &mut PrirodaCompilerCalls {
                            step_count,
                            config,
                            receiver,
                            skip_to_main: stops_at_start.is_none(),
                        },
                    )
                    .run()
                });
            })
            .join();
            std::thread::sleep(std::time::Duration::from_millis(200));
        }
        println!("\n============== Miri crashed too often. Aborting ==============\n");
    });
    server(sender);
    handle.join().unwrap();
}
