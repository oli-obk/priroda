#![feature(rustc_private, custom_attribute, decl_macro)]
#![allow(unused_attributes)]
#![recursion_limit = "5000"]

extern crate getopts;
extern crate miri;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_data_structures;
extern crate rustc_const_math;
extern crate graphviz as dot;
extern crate env_logger;
extern crate log_settings;
extern crate log;
extern crate syntax;
extern crate syntax_pos;
extern crate hyper;
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
use render::Renderer;

use horrorshow::prelude::*;
use promising_future::future_promise;

use miri::{
    StackPopCleanup,
    AllocId,
    Place,
};
use step::{BreakpointTree, Breakpoint};

use rustc_data_structures::indexed_vec::Idx;
use rustc::session::Session;
use rustc::ty::{self, TyCtxt, ParamEnv};
use rustc::hir::def_id::{CrateNum, DefId, DefIndex, DefIndexAddressSpace};
use rustc::mir;
use rustc_driver::{driver, CompilerCalls};

use std::sync::Mutex;
use hyper::server::{Request, Response};
use futures::future::FutureResult;

fn should_hide_stmt(stmt: &mir::Statement) -> bool {
    use rustc::mir::StatementKind::*;
    match stmt.kind {
        StorageLive(_) | StorageDead(_) | Validate(_, _) | EndRegion(_) | Nop => true,
        _ => false,
    }
}

type EvalContext<'a, 'tcx> = miri::EvalContext<'a, 'tcx, 'tcx, miri::Evaluator<'tcx>>;

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
            act(state.session, state.tcx.unwrap());
        });

        control
    }
}

fn load_breakpoints_from_file() -> BreakpointTree {
    use std::io::{BufRead, BufReader};
    use std::fs::File;
    match File::open("./.priroda_breakpoints") {
        Ok(file) => {
            let file = BufReader::new(file);
            let mut breakpoints = BreakpointTree::new();
            for line in file.lines() {
                let line = line.expect("Couldn't read breakpoint from file");
                if line.trim().is_empty() {
                    continue;
                }
                breakpoints.add_breakpoint(parse_breakpoint_from_url(&format!("/set/{}", line)).unwrap());
            }
            breakpoints
        }
        Err(e) => {
            eprintln!("Couldn't load breakpoint file ./.priroda_breakpoints: {:?}", e);
            BreakpointTree::new()
        }
    }
}

fn parse_breakpoint_from_url(s: &str) -> Result<Breakpoint, String> {
    let regex = regex::Regex::new(r#"/\w+/DefId\((\d+)/(0|1):(\d+) ~ [^\)]+\)@(\d+):(\d+)"#).unwrap();
    // my_command/DefId(1/0:14824 ~ mycrate::main)@1:3
    //                  ^ ^ ^                      ^ ^
    //                  | | |                      | statement
    //                  | | |                      BasicBlock
    //                  | | DefIndex::as_array_index()
    //                  | DefIndexAddressSpace
    //                  CrateNum

    let s = s.replace("%20", " ");
    let caps = regex.captures(&s).ok_or_else(||format!("Invalid breakpoint {}", s))?;

    // Parse DefId
    let crate_num = CrateNum::new(caps.get(1).unwrap().as_str().parse::<usize>().unwrap());
    let address_space = match caps.get(2).unwrap().as_str().parse::<u64>().unwrap() {
        0 => DefIndexAddressSpace::Low,
        1 => DefIndexAddressSpace::High,
        _ => return Err("address_space is not 0 or 1".to_string()),
    };
    let index = caps.get(3).unwrap().as_str().parse::<usize>().map_err(|_| "index is not a positive integer")?;
    let def_index = DefIndex::from_array_index(index, address_space);
    let def_id = DefId {
        krate: crate_num,
        index: def_index,
    };

    // Parse block and stmt
    let bb = mir::BasicBlock::new(caps.get(4).unwrap().as_str().parse::<usize>().map_err(|_| "block id is not a positive integer")?);
    let stmt = caps.get(5).unwrap().as_str().parse::<usize>().map_err(|_| "stmt id is not a positive integer")?;

    Ok(Breakpoint(def_id, bb, stmt))
}

fn create_ecx<'a, 'tcx: 'a>(session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>) -> EvalContext<'a, 'tcx> {
    let (node_id, span) = session.entry_fn.borrow().expect("no main or start function found");
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

fn act<'a, 'tcx: 'a>(session: &Session, tcx: TyCtxt<'a, 'tcx, 'tcx>) {
    let mut ecx = create_ecx(session, tcx);
    let mut breakpoints = load_breakpoints_from_file();

    // setup http server and similar
    let (sender, receiver) = std::sync::mpsc::channel();
    let sender = Mutex::new(sender);

    let handle = std::thread::spawn(|| {
        // setup server
        let port = ::std::env::var("PORT").unwrap_or_else(|_|"54321".to_string());
        let is_heroku = ::std::env::var("IS_ON_HEROKU").map_err(|_|()).and_then(|s|str::parse(&s).map_err(|_|())).unwrap_or(false);
        let addr = if is_heroku {
            format!("0.0.0.0:{}", port)
        } else {
            format!("127.0.0.1:{}", port)
        }.parse().unwrap();
        let server = hyper::server::Http::new().bind(&addr, move || {
            Ok(Service(sender.lock().unwrap().clone()))
        }).expect("could not create http server");
        let addr = format!("http://{}", server.local_addr().unwrap());
        if is_heroku || open::that(&addr).is_err() {
            println!("open {} in your browser", addr);
        };
        server.run().unwrap()
    });

    // process commands
    for (path, promise) in receiver {
        macro renderer() {
            Renderer::new(promise, &ecx, tcx, session, &breakpoints)
        }
        macro render_main_window($frame:expr, $msg:expr) {
            renderer!().render_main_window($frame, $msg);
        }

        println!("processing `{}`", path);
        assert_eq!(&path[..1], "/");
        let mut matches = path[1..].split('/');
        match matches.next() {
            Some("") | None => render_main_window!(None, String::new()),
            Some("reverse_ptr") => renderer!().render_reverse_ptr(matches.next().map(str::parse)),
            Some("ptr") => renderer!().render_ptr_memory(matches.next().map(|id|Ok(AllocId(id.parse::<u64>()?))), matches.next().map(str::parse)),
            Some("frame") => match matches.next().map(str::parse) {
                Some(Ok(n)) => render_main_window!(Some(n), String::new()),
                Some(Err(e)) => render_main_window!(None, format!("not a number: {:?}", e)),
                // display current frame
                None => render_main_window!(None, String::new()),
            },
            Some("add_breakpoint") => {
                let res = parse_breakpoint_from_url(&path);
                match res {
                    Ok(breakpoint) => {
                        breakpoints.add_breakpoint(breakpoint);
                        render_main_window!(None, format!("Breakpoint added for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2));
                    }
                    Err(e) => {
                        render_main_window!(None, e.to_string());
                    }
                }
            }
            Some("add_breakpoint_here") => {
                let frame = ecx.frame();
                breakpoints.add_breakpoint(Breakpoint(frame.instance.def_id(), frame.block, frame.stmt));
                render_main_window!(None, format!("Breakpoint added for {:?}@{}:{}", frame.instance.def_id(), frame.block.index(), frame.stmt));
            }
            Some("remove_breakpoint") => {
                let res = parse_breakpoint_from_url(&path);
                match res {
                    Ok(breakpoint) => {
                        if breakpoints.remove_breakpoint(breakpoint) {
                            render_main_window!(None, format!("Breakpoint removed for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2));
                        } else {
                            render_main_window!(None, format!("No breakpoint for for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2));
                        }
                    }
                    Err(e) => {
                        render_main_window!(None, e.to_string());
                    }
                }
            }
            Some("remove_all_breakpoints") => {
                breakpoints.remove_all();
                render_main_window!(None, format!("All breakpoints removed"));
            }
            Some("restart") => {
                ecx = create_ecx(session, tcx);
                render_main_window!(None, String::new());
            }
            Some(cmd) => {
                if let Some(message) = ::step::step_command(&mut ecx, &breakpoints, cmd) {
                    render_main_window!(None, message);
                } else {
                    render_main_window!(None, format!("unknown command: {}", cmd));
                }
            }
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
