#![feature(rustc_private, custom_attribute, decl_macro, plugin, fnbox)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate getopts;
extern crate miri;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_data_structures;
extern crate graphviz as dot;
extern crate env_logger;
extern crate log_settings;
extern crate log;
extern crate syntax;
extern crate syntax_pos;
extern crate futures;
extern crate open;
extern crate promising_future;
extern crate syntect;
#[macro_use]
extern crate horrorshow;
extern crate cgraph;
extern crate regex;
#[macro_use]
extern crate lazy_static;

mod render;
mod step;

use std::boxed::FnBox;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use rustc::session::Session;
use rustc::ty::{self, TyCtxt, ParamEnv};
use rustc::mir;
use rustc_driver::{driver, CompilerCalls};

use rocket::State;
use rocket::fairing::AdHoc;
use rocket::response::{Flash, Redirect};
use rocket::response::content::*;
use rocket::response::status::BadRequest;
use promising_future::future_promise;

use miri::{
    StackPopCleanup,
    AllocId,
    Place,
};

use step::BreakpointTree;

fn should_hide_stmt(stmt: &mir::Statement) -> bool {
    use rustc::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Validate(_, _) | EndRegion(_) | Nop => true,
        _ => false,
    }
}

type EvalContext<'a, 'tcx> = miri::EvalContext<'a, 'tcx, 'tcx, miri::Evaluator<'tcx>>;

pub struct PrirodaContext<'a, 'tcx: 'a> {
    ecx: EvalContext<'a, 'tcx>,
    bptree: BreakpointTree,
    step_count: &'a mut u128,
}

type RResult<T> = Result<T, Html<String>>;

struct MiriCompilerCalls(Arc<Mutex<u128>>, Arc<Mutex<::std::sync::mpsc::Receiver<Box<FnBox(&mut PrirodaContext) + Send>>>>);

impl<'a> CompilerCalls<'a> for MiriCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        let step_count = self.0.clone();
        let receiver = self.1.clone();
        control.after_analysis.callback = Box::new(move |state| {
            state.session.abort_if_errors();
            let mut step_count = step_count.lock().unwrap_or_else(|err|err.into_inner());

            let mut pcx = PrirodaContext{
                ecx: create_ecx(state.session, state.tcx.unwrap()),
                bptree: step::load_breakpoints_from_file(),
                step_count: &mut *step_count,
            };

            // Step to the position where miri crashed if it crashed
            for _ in 0..*pcx.step_count {
                match pcx.ecx.step() {
                    Ok(true) => {}
                    res => panic!("Miri is not deterministic causing error {:?}", res),
                }
            }

            // Just ignore poisoning by panicking
            let receiver = receiver.lock().unwrap_or_else(|err|err.into_inner());

            // process commands
            for command in receiver.iter() {
                command.call_box((&mut pcx,));
            }
        });

        control
    }
}

fn create_ecx<'a, 'tcx: 'a>(session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>) -> EvalContext<'a, 'tcx> {
    let (node_id, span, _) = session.entry_fn.borrow().expect("no main or start function found");
    let main_id = tcx.hir.local_def_id(node_id);

    let main_instance = ty::Instance::mono(tcx, main_id);

    let mut ecx = EvalContext::new(tcx.at(span), ParamEnv::reveal_all(), Default::default(), Default::default());
    let main_mir = ecx.load_mir(main_instance.def).expect("mir for `main` not found");

    let return_type = main_mir.return_ty();
    let return_layout = tcx.layout_of(ParamEnv::reveal_all().and(return_type)).expect("couldnt get layout for return pointer");
    let return_ptr = ecx.memory.allocate(return_layout.size, return_layout.align, None).unwrap();
    ecx.push_stack_frame(
        main_instance,
        span,
        main_mir,
        Place::from_ptr(return_ptr, return_layout.align),
        StackPopCleanup::None,
    ).unwrap();
    ecx
}

pub struct PrirodaSender(Mutex<::std::sync::mpsc::Sender<Box<FnBox(&mut PrirodaContext) + Send>>>);

impl PrirodaSender {
    fn do_work<'r, T, F>(&self, f: F) -> Result<T, Html<String>>
        where T: rocket::response::Responder<'r> + Send + 'static,
              F: FnOnce(&mut PrirodaContext) -> T + Send + 'static {
        let (future, promise) = future_promise();
        let sender = self.0.lock().unwrap_or_else(|err|err.into_inner());
        match sender.send(Box::new(move |pcx: &mut PrirodaContext| {
            promise.set(f(pcx));
        })) {
            Ok(()) => match future.value() {
                Some(val) => Ok(val),
                None => Err(Html("<center><h1>Miri crashed please go to <a href='/'>index</a></h1></center>".to_string()))
            },
            Err(_) => {
                Err(Html("<center><h1>Miri crashed too often. Please restart priroda.</h1></center>".to_string()))
            }
        }
    }

    fn do_work_and_redirect<'r, F>(&self, f: F) -> Result<Flash<Redirect>, Html<String>>
        where F: FnOnce(&mut PrirodaContext) -> String + Send + 'static {
        self.do_work(move |pcx| {
            Flash::success(Redirect::to("/"), f(pcx))
        })
    }
}

macro action_route($name:ident: $route:expr, |$pcx:ident $(,$arg:ident : $arg_ty:ty)*| $body:block) {
    use rocket::State;
    use rocket::response::{Flash, Redirect};
    #[get($route)]
    pub fn $name(sender: State<::PrirodaSender> $(,$arg:$arg_ty)*) -> ::RResult<Flash<Redirect>> {
        sender.do_work_and_redirect(move |$pcx| {
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

#[get("/favicon.ico")]
fn favicon() -> rocket::response::Content<Vec<u8>> {
    Content(rocket::http::ContentType::BMP, ::std::fs::read("./resources/favicon.ico").unwrap())
}

#[cfg(not(feature="static_resources"))]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<Result<Css<String>, JavaScript<String>>, std::io::Error> {
    use std::io::{Error, ErrorKind};
    let mut res_path = PathBuf::from("./resources/");
    res_path.push(path);
    let content = ::std::fs::read_to_string(&res_path)?;
    match res_path.extension().and_then(|ext|ext.to_str()) {
        Some("css") => Ok(Ok(Css(content))),
        Some("js") => Ok(Err(JavaScript(content))),
        _ => Err(Error::new(ErrorKind::InvalidInput, "Invalid extension")),
    }
}

#[cfg(feature="static_resources")]
#[get("/resources/<path..>")]
fn resources(path: PathBuf) -> Result<Result<Css<&'static str>, JavaScript<&'static str>>, std::io::Error> {
    use std::io::{Error, ErrorKind};
    match path.as_os_str().to_str() {
        Some("svg-pan-zoom.js") => Ok(Err(JavaScript(include_str!("../resources/svg-pan-zoom.js")))),
        Some("zoom_mir.js") => Ok(Err(JavaScript(include_str!("../resources/zoom_mir.js")))),
        Some("style.css") => Ok(Ok(Css(include_str!("../resources/style.css")))),
        Some("positioning.css") => Ok(Ok(Css(include_str!("../resources/positioning.css")))),
        _ => Err(Error::new(ErrorKind::InvalidInput, "Unknown resource")),
    }
}

fn server(sender: PrirodaSender) {
    use rocket::config::Value;
    rocket::ignite()
        .manage(sender)
        .mount("/", routes![please_panic, favicon, resources])
        .mount("/", render::routes::routes())
        .mount("breakpoints", step::bp_routes::routes())
        .mount("step", step::step_routes::routes())
        .attach(AdHoc::on_launch(|rocket| {
            let config = rocket.config();
            if config.extras.get("spawn_browser") == Some(&Value::Boolean(true)) {
                let addr = format!("http://{}:{}", config.address, config.port);
                if open::that(&addr).is_err() {
                    println!("open {} in your browser", addr);
                }
            }
        }))
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
        _ => {
            option_env!("RUST_SYSROOT")
                .expect(
                    "need to specify RUST_SYSROOT env var or use rustup or multirust",
                )
                .to_owned()
        }
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

    let handle = std::thread::spawn(move || {
        let args = Arc::new(args);
        let receiver = Arc::new(Mutex::new(receiver));
        for i in 0..5 {
            if i != 0 {
                println!("\n============== Miri crashed - restart try {} ==============\n", i);
            }
            let step_count = step_count.clone();
            let receiver = receiver.clone();
            let args = args.clone();
            // Ignore result to restart in case of a crash
            let _ = std::thread::spawn(move || {
                rustc_driver::run_compiler(&*args, &mut MiriCompilerCalls(step_count, receiver), None, None);
            }).join();
            std::thread::sleep(std::time::Duration::from_millis(200));
        }
        println!("\n============== Miri crashed too often. Aborting ==============\n");
    });
    server(sender);
    handle.join().unwrap();
}

fn init_logger() {
    const NSPACES: usize = 40;
    let format = |record: &log::LogRecord| {
        // prepend spaces to indent the final string
        let indentation = log_settings::settings().indentation;
        format!("{lvl}:{module}{depth:2}{indent:<indentation$} {text}",
            lvl = record.level(),
            module = record.location().module_path(),
            depth = indentation / NSPACES,
            indentation = indentation % NSPACES,
            indent = "",
            text = record.args())
    };

    let mut builder = env_logger::LogBuilder::new();
    builder.format(format).filter(None, log::LogLevelFilter::Info);

    if std::env::var("MIRI_LOG").is_ok() {
        builder.parse(&std::env::var("MIRI_LOG").unwrap());
    }

    builder.init().unwrap();
}
