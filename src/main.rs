#![feature(rustc_private, custom_attribute)]
#![feature(pub_restricted)]
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
#[macro_use]
extern crate log;
extern crate syntax;
extern crate hyper;
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
    Pointer,
};

use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls};
use rustc::ty::TyCtxt;

use std::sync::Mutex;
use hyper::server::{Server, Request, Response};
use hyper::header::{TransferEncoding, Encoding, ContentType};
use hyper::uri::RequestUri;

enum Page {
    Html(Box<RenderBox + Send>),
    Ico,
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
            let def_id = tcx.map.local_def_id(node_id);
            debug!("found `main` function at: {:?}", span);

            let mir = tcx.item_mir(def_id);
            let def_id = tcx.map.local_def_id(node_id);

            let memory_size = 100*1024*1024; // 100MB
            let stack_limit = 100;
            let mut ecx = EvalContext::new(tcx, memory_size, stack_limit);
            let substs = tcx.intern_substs(&[]);

            ecx.push_stack_frame(
                def_id,
                mir.span,
                mir,
                substs,
                Lvalue::from_ptr(Pointer::zst_ptr()),
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
    // weird hack to make sure the closure passed to the server doesn't consume the sender
    let sender = Mutex::new(sender);

    // setup server
    let addr = "localhost:54321";
    let server = Server::http(addr).expect("could not create http server");
    let addr = format!("http://{}", addr);
    if open::that(&addr).is_err() {
        println!("open {} in your browser", addr);
    };
    let handle = std::thread::spawn(|| {
        server.handle(move |req: Request, mut res: Response| {
            if let RequestUri::AbsolutePath(path) = req.uri {
                let (future, promise) = future_promise::<Page>();
                println!("got `{}`", path);
                sender.lock().unwrap().send((path, promise)).unwrap();
                match future.value() {
                    Some(Html(output)) => {
                        res.headers_mut().set(
                            TransferEncoding(vec![
                                Encoding::Gzip,
                                Encoding::Chunked,
                            ])
                        );
                        res.headers_mut().set(ContentType::html());
                        // hack, because `Box<RenderBox+Send>` isn't the same as `Box<RenderBox>`
                        let output: Box<RenderBox> = output;
                        output.write_to_io(&mut res.start().unwrap()).unwrap();
                    }
                    Some(Ico) => {
                        let ico = include_bytes!("../favicon.ico");
                        use hyper::mime::{Mime, TopLevel, SubLevel};
                        res.headers_mut().set(ContentType(Mime(TopLevel::Image, SubLevel::Ext("x-icon".to_string()), vec![])));
                        res.send(ico).unwrap()
                    }
                    None => res.send(b"unable to process").unwrap(),
                }
            }
        }).expect("http server crashed");
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
            Some("favicon.ico") => promise.set(Ico),
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
