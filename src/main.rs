#![feature(
    rustc_private,
    decl_macro,
    plugin,
    try_blocks,
    proc_macro_hygiene
)]
#![feature(never_type)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]

extern crate rustc_middle;
extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_mir;
extern crate rustc_span;

extern crate lazy_static;
extern crate regex;
#[macro_use]
extern crate rental;
extern crate miri;
#[macro_use]
extern crate rocket;

extern crate env_logger;
extern crate log;
extern crate log_settings;

extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

extern crate open;
extern crate promising_future;
extern crate syntect;
#[macro_use]
extern crate horrorshow;
extern crate cgraph;

mod render;
mod step;
mod watch;

use std::ops::FnOnce;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use rustc_middle::mir;
use rustc_middle::ty::TyCtxt;
use rustc_driver::Compilation;
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_interface::interface;

use promising_future::future_promise;
use rocket::response::content::*;
use rocket::response::status::BadRequest;
use rocket::response::NamedFile;
use rocket::State;

use miri::AllocId;

use crate::step::BreakpointTree;

fn should_hide_stmt(stmt: &mir::Statement) -> bool {
    use rustc_middle::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Nop => true,
        _ => false,
    }
}

type InterpCx<'tcx> = miri::InterpCx<'tcx, 'tcx, miri::Evaluator<'tcx, 'tcx>>;

pub struct PrirodaContext<'a, 'tcx: 'a> {
    ecx: InterpCx<'tcx>,
    step_count: &'a mut u128,
    traces: watch::Traces<'tcx>,
    config: &'a mut Config,
}

impl<'a, 'tcx: 'a> PrirodaContext<'a, 'tcx> {
    fn restart(&mut self) {
        self.ecx = create_ecx(self.ecx.tcx.tcx);
        *self.step_count = 0;
        self.traces.clear(); // Cleanup all traces
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
        main_id,
        miri::MiriConfig {
            args: vec![],
            communicate: true,
            excluded_env_vars: vec![],
            ignore_leaks: true,
            seed: None,
            tracked_pointer_tag: None,
            tracked_alloc_id: None,
            validate: true,
            stacked_borrows: true,
        },
    )
    .unwrap()
    .0
}

pub struct PrirodaSender(
    Mutex<::std::sync::mpsc::Sender<Box<dyn FnOnce(&mut PrirodaContext) + Send>>>,
);

impl PrirodaSender {
    fn do_work<'r, T, F>(&self, f: F) -> Result<T, Html<String>>
    where
        T: rocket::response::Responder<'r> + Send + 'static,
        F: FnOnce(&mut PrirodaContext) -> T + Send + 'static,
    {
        let (future, promise) = future_promise();
        let sender = self.0.lock().unwrap_or_else(|err| err.into_inner());
        match sender.send(Box::new(move |pcx: &mut PrirodaContext| {
            promise.set(f(pcx));
        })) {
            Ok(()) => match future.value() {
                Some(val) => Ok(val),
                None => Err(Html(
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

macro action_route($name:ident : $route:expr, |$pcx:ident $(,$arg:ident : $arg_ty:ty)*| $body:block) {
    #[get($route)]
    pub fn $name(
        sender: rocket::State<crate::PrirodaSender>
        $(,$arg:$arg_ty)*
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |$pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), (||$body)())
        })
    }
}

macro view_route($name:ident : $route:expr, |$pcx:ident $(,$arg:ident : $arg_ty:ty)*| $body:block) {
    #[get($route)]
    pub fn $name(
        sender: rocket::State<crate::PrirodaSender>
        $(,$arg:$arg_ty)*
    ) -> crate::RResult<Html<String>> {
        sender.do_work(move |pcx| {
            let $pcx = &*pcx;
            (||$body)()
        })
    }
}

#[get("/please_panic")]
#[allow(unreachable_code)]
fn please_panic(sender: State<PrirodaSender>) -> RResult<()> {
    sender.do_work(|_pcx| {
        panic!("You requested a panic");
    })
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
fn step_count(sender: State<PrirodaSender>) -> RResult<String> {
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

    // Taken from https://github.com/Manishearth/rust-clippy/pull/911.
    let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
    let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => option_env!("RUST_SYSROOT")
            .expect("need to specify RUST_SYSROOT env var or use rustup or multirust")
            .to_owned(),
    }
}

fn main() {
    init_logger();
    let mut args: Vec<String> = std::env::args().collect();

    let sysroot_flag = String::from("--sysroot");
    if !args.contains(&sysroot_flag) {
        args.push(sysroot_flag);
        args.push(find_sysroot());
    }

    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
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
                    struct PrirodaCompilerCalls {
                        step_count: Arc<Mutex<u128>>,
                        config: Arc<Mutex<Config>>,
                        receiver: Arc<
                            Mutex<
                                std::sync::mpsc::Receiver<
                                    Box<dyn FnOnce(&mut PrirodaContext) + Send>,
                                >,
                            >,
                        >,
                    }

                    impl rustc_driver::Callbacks for PrirodaCompilerCalls {
                        fn after_analysis<'tcx>(&mut self, compiler: &interface::Compiler, queries: &'tcx rustc_interface::Queries<'tcx>) -> Compilation {
                            compiler.session().abort_if_errors();

                            queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
                                let mut step_count = self
                                    .step_count
                                    .lock()
                                    .unwrap_or_else(|err| err.into_inner());
                                let mut config =
                                    self.config.lock().unwrap_or_else(|err| err.into_inner());

                                let mut pcx = PrirodaContext {
                                    ecx: create_ecx(tcx),
                                    step_count: &mut *step_count,
                                    traces: watch::Traces::new(),
                                    config: &mut *config,
                                };

                                // Step to the position where miri crashed if it crashed
                                for _ in 0..*pcx.step_count {
                                    match pcx.ecx.step() {
                                        Ok(true) => {}
                                        res => panic!(
                                            "Miri is not deterministic causing error {:?}",
                                            res
                                        ),
                                    }
                                }

                                // Just ignore poisoning by panicking
                                let receiver =
                                    self.receiver.lock().unwrap_or_else(|err| err.into_inner());

                                // process commands
                                for command in receiver.iter() {
                                    command(&mut pcx);
                                }
                            });

                            compiler.session().abort_if_errors();

                            Compilation::Stop
                        }
                    }

                    rustc_driver::run_compiler(
                        &*args,
                        &mut PrirodaCompilerCalls {
                            step_count,
                            config,
                            receiver,
                        },
                        None,
                        None,
                    )
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

fn init_logger() {
    const NSPACES: usize = 40;
    let format = |_fmt: &mut _, record: &log::Record| {
        // prepend spaces to indent the final string
        let indentation = log_settings::settings().indentation;
        println!(
            "{lvl}:{module}{depth:2}{indent:<indentation$} {text}",
            lvl = record.level(),
            module = record.module_path().unwrap_or(""),
            depth = indentation / NSPACES,
            indentation = indentation % NSPACES,
            indent = "",
            text = record.args()
        );
        Ok(())
    };

    let mut builder = env_logger::Builder::new();
    builder.format(format).filter(None, log::LevelFilter::Info);

    if std::env::var("MIRI_LOG").is_ok() {
        builder.parse(&std::env::var("MIRI_LOG").unwrap());
    }

    builder.init();
}
