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
use rocket::State;
use rocket::fairing::AdHoc;
use rocket::response::{Flash, Redirect};
use rocket::response::content::*;
use rocket::response::status::*;
use promising_future::future_promise;

use miri::{
    StackPopCleanup,
    AllocId,
    Place,
};
use step::BreakpointTree;

use rustc::session::Session;
use rustc::ty::{self, TyCtxt, ParamEnv};
use rustc::mir;
use rustc_driver::{driver, CompilerCalls};

use std::sync::Mutex;

fn should_hide_stmt(stmt: &mir::Statement) -> bool {
    use rustc::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Validate(_, _) | EndRegion(_) | Nop => true,
        _ => false,
    }
}

type EvalContext<'a, 'tcx> = miri::EvalContext<'a, 'tcx, 'tcx, miri::Evaluator<'tcx>>;

type PrirodaSender = Mutex<::std::sync::mpsc::Sender<Box<FnBox(&mut PrirodaContext) + Send>>>;

pub struct PrirodaContext<'a, 'tcx: 'a> {
    ecx: EvalContext<'a, 'tcx>,
    bptree: BreakpointTree,
    step_count: u128,
}

impl<'a, 'tcx: 'a> std::ops::Deref for PrirodaContext<'a, 'tcx> {
    type Target = EvalContext<'a, 'tcx>;

    fn deref(&self) -> &Self::Target {
        &self.ecx
    }
}

impl<'a, 'tcx: 'a> std::ops::DerefMut for PrirodaContext<'a, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ecx
    }
}

struct MiriCompilerCalls;

impl<'a> CompilerCalls<'a> for MiriCompilerCalls {
    fn build_controller(
        &mut self,
        _: &Session,
        _: &getopts::Matches
    ) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        control.after_analysis.callback = Box::new(|state| {
            state.session.abort_if_errors();
            let ecx = create_ecx(state.session, state.tcx.unwrap());
            let bptree = step::load_breakpoints_from_file();
            let pcx = PrirodaContext{
                ecx,
                bptree,
                step_count: 0
            };
            act(pcx);
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
    let return_ptr = ecx.memory.allocate(return_layout.size.bytes(), return_layout.align, None).unwrap();
    ecx.push_stack_frame(
        main_instance,
        span,
        main_mir,
        Place::from_ptr(return_ptr, return_layout.align),
        StackPopCleanup::None,
    ).unwrap();
    ecx
}

macro do_work($sender:expr, |$pcx:ident| $body:block) {{
    let (future, promise) = future_promise();
    $sender.lock().unwrap().send(Box::new(move |$pcx: &mut PrirodaContext| {
        promise.set({$body});
    })).unwrap();
    future.value().unwrap()
}}

macro do_work_and_redirect($sender:expr, |$pcx:ident| $body:block) {
    do_work!($sender, |$pcx| {
        Flash::success(Redirect::to("/"), {$body})
    })
}

#[get("/")]
fn index(flash: Option<rocket::request::FlashMessage>, sender: State<PrirodaSender>) -> Html<String> {
    do_work!(sender, |pcx| {
        if let Some(flash) = flash {
            render::set_flash_message(flash.msg().to_string());
        }
        render::render_main_window(pcx, None)
    })
}

#[get("/frame/<frame>")]
fn frame(sender: State<PrirodaSender>, frame: usize) -> Html<String> {
    do_work!(sender, |pcx| {
        render::render_main_window(pcx, Some(frame))
    })
}

#[get("/frame/<frame>", rank = 42)] // Error handler
fn frame_invalid(frame: String) -> BadRequest<String> {
    BadRequest(Some(format!("not a number: {:?}", frame.parse::<usize>().unwrap_err())))
}

#[get("/ptr/<path..>")]
fn ptr(path: PathBuf, sender: State<PrirodaSender>) -> Html<String> {
    do_work!(sender, |pcx| {
        let path = path.to_string_lossy();
        let mut matches = path.split('/');
        render::render_ptr_memory(pcx, matches.next().map(|id|Ok(AllocId(id.parse::<u64>()?))), matches.next().map(str::parse))
    })
}

#[get("/reverse_ptr/<ptr>")]
fn reverse_ptr(ptr: String, sender: State<PrirodaSender>) -> Html<String> {
    do_work!(sender, |pcx| {
        render::render_reverse_ptr(pcx, Some(ptr.parse()))
    })
}

fn act<'a, 'tcx: 'a>(mut pcx: PrirodaContext<'a, 'tcx>) {
    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
    let sender: Mutex<::std::sync::mpsc::Sender<Box<FnBox(&mut PrirodaContext) + Send>>> = Mutex::new(sender);

    let handle = std::thread::spawn(|| {
        use rocket::config::Value;
        rocket::ignite()
            .manage(sender)
            .mount("/", routes![
                index,
                frame, frame_invalid,
                ptr,
                reverse_ptr,
            ])
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
    });

    // process commands
    for command in receiver {
        command.call_box((&mut pcx,));
    }
    handle.join().unwrap();
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

    rustc_driver::run_compiler(&args, &mut MiriCompilerCalls, None, None);
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
