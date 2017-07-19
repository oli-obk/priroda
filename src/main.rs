#![feature(rustc_private, custom_attribute)]
#![feature(i128_type)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]

extern crate getopts;
extern crate miri;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_trans;
extern crate rustc_data_structures;
extern crate graphviz as dot;
extern crate env_logger;
extern crate log_settings;
extern crate log;
extern crate syntax;
extern crate hyper;
extern crate futures;
extern crate open;
extern crate promising_future;
#[macro_use]
extern crate horrorshow;
extern crate cgraph;

mod graphviz;
mod commands;

use commands::Renderer;

use horrorshow::prelude::*;
use promising_future::future_promise;

use miri::{
    EvalContext,
    StackPopCleanup,
    Lvalue,
};

use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};
use rustc::ty::{self, TyCtxt};
use syntax::ast::{MetaItemKind, NestedMetaItemKind};

use std::sync::Mutex;
use hyper::server::{Request, Response};
use futures::future::FutureResult;

enum Page {
    Html(Box<RenderBox + Send>),
}

use Page::*;

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
            let tcx = state.tcx.unwrap();

            let (node_id, span) = state.session.entry_fn.borrow().expect("no main or start function found");
            let main_id = tcx.hir.local_def_id(node_id);

            let main_instance = ty::Instance::mono(tcx, main_id);

            let limits = resource_limits_from_attributes(state);
            let mut ecx = EvalContext::new(tcx, limits);
            let main_mir = ecx.load_mir(main_instance.def).expect("mir for `main` not found");

            ecx.push_stack_frame(
                main_instance,
                span,
                main_mir,
                Lvalue::undef(),
                StackPopCleanup::None,
            ).unwrap();

            act(ecx, state.session, tcx);
        });

        control
    }
}

fn act<'a, 'tcx: 'a>(mut ecx: EvalContext<'a, 'tcx>, session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>) {
    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
    let sender = Mutex::new(sender);

    let handle = std::thread::spawn(|| {
        // setup server
        let addr = "127.0.0.1:54321".parse().unwrap();
        let server = hyper::server::Http::new().bind(&addr, move || {
            Ok(Service(sender.lock().unwrap().clone()))
        }).expect("could not create http server");
        let addr = format!("http://{}", server.local_addr().unwrap());
        if open::that(&addr).is_err() {
            println!("open {} in your browser", addr);
        };
        server.run().unwrap()
    });

    // process commands
    for (path, promise) in receiver {
        println!("processing `{}`", path);
        assert_eq!(&path[..1], "/");
        let mut matches = path[1..].split('/');
        match matches.next() {
            Some("") | None => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, String::new()),
            Some("step") => match ecx.step() {
                Ok(true) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, String::new()),
                Ok(false) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, "interpretation finished".to_string()),
                Err(e) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("{:?}", e)),
            },
            Some("next") => {
                let frame = ecx.stack().len();
                let stmt = ecx.stack().last().unwrap().stmt;
                let block = ecx.stack().last().unwrap().block;
                loop {
                    match ecx.step() {
                        Ok(true) => if ecx.stack().len() == frame && (block < ecx.stack().last().unwrap().block || stmt < ecx.stack().last().unwrap().stmt) {
                            Renderer::new(promise, &ecx, tcx, session).render_main_window(None, String::new());
                        } else {
                            continue;
                        },
                        Ok(false) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, "interpretation finished".to_string()),
                        Err(e) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("{:?}", e)),
                    }
                    break;
                }
            },
            Some("return") => {
                let frame = ecx.stack().len();
                fn is_ret(ecx: &EvalContext) -> bool {
                    let stack = ecx.stack().last().unwrap();
                    let basic_block = &stack.mir.basic_blocks()[stack.block];

                    match basic_block.terminator().kind {
                        rustc::mir::TerminatorKind::Return => stack.stmt >= basic_block.statements.len(),
                        _ => false,
                    }
                }
                loop {
                    if ecx.stack().len() <= frame && is_ret(&ecx) {
                        Renderer::new(promise, &ecx, tcx, session).render_main_window(None, String::new());
                        break;
                    }
                    match ecx.step() {
                        Ok(true) => continue,
                        Ok(false) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, "interpretation finished".to_string()),
                        Err(e) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("{:?}", e)),
                    }
                    break;
                }
            }
            Some("continue") => {
                loop {
                    match ecx.step() {
                        Ok(true) => continue,
                        Ok(false) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, "interpretation finished".to_string()),
                        Err(e) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("{:?}", e)),
                    }
                    break;
                }
            },
            Some("reverse_ptr") => Renderer::new(promise, &ecx, tcx, session).render_reverse_ptr(matches.next().map(str::parse)),
            Some("ptr") => Renderer::new(promise, &ecx, tcx, session).render_ptr_memory(matches.next().map(str::parse), matches.next().map(str::parse)),
            Some("frame") => match matches.next().map(str::parse) {
                Some(Ok(n)) => Renderer::new(promise, &ecx, tcx, session).render_main_window(Some(n), String::new()),
                Some(Err(e)) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("not a number: {:?}", e)),
                // display current frame
                None => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, String::new()),
            },
            Some(cmd) => Renderer::new(promise, &ecx, tcx, session).render_main_window(None, format!("unknown command: {}", cmd)),
        }
    }
    handle.join().unwrap();
}

struct Service(std::sync::mpsc::Sender<(String, promising_future::Promise<Page>)>);

impl hyper::server::Service for Service {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = FutureResult<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        let (future, promise) = future_promise::<Page>();
        self.0.send((req.path().to_string(), promise)).unwrap();
        println!("got `{}`", req.path());
        futures::future::ok(match future.value() {
            Some(Html(output)) => {
                println!("rendering page");
                let mut text = Vec::new();
                output.write_to_io(&mut text).unwrap();
                println!("sending page");
                Response::new()
                    .with_header(hyper::header::ContentLength(text.len() as u64))
                    .with_body(text)
            }
            None => Response::new()
                    .with_status(hyper::StatusCode::NotFound)
        })
    }
}

fn resource_limits_from_attributes(state: &driver::CompileState) -> miri::ResourceLimits {
    let mut limits = miri::ResourceLimits::default();
    let krate = state.hir_crate.as_ref().unwrap();
    let err_msg = "miri attributes need to be in the form `miri(key = value)`";
    let extract_int = |lit: &syntax::ast::Lit| -> u128 {
        match lit.node {
            syntax::ast::LitKind::Int(i, _) => i,
            _ => state.session.span_fatal(lit.span, "expected an integer literal"),
        }
    };

    for attr in krate.attrs.iter().filter(|a| a.name().map_or(false, |n| n == "miri")) {
        if let Some(items) = attr.meta_item_list() {
            for item in items {
                if let NestedMetaItemKind::MetaItem(ref inner) = item.node {
                    if let MetaItemKind::NameValue(ref value) = inner.node {
                        match &inner.name().as_str()[..] {
                            "memory_size" => limits.memory_size = extract_int(value) as u64,
                            "step_limit" => limits.step_limit = extract_int(value) as u64,
                            "stack_limit" => limits.stack_limit = extract_int(value) as usize,
                            _ => state.session.span_err(item.span, "unknown miri attribute"),
                        }
                    } else {
                        state.session.span_err(inner.span, err_msg);
                    }
                } else {
                    state.session.span_err(item.span, err_msg);
                }
            }
        } else {
            state.session.span_err(attr.span, err_msg);
        }
    }
    limits
}

fn find_sysroot() -> String {
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
