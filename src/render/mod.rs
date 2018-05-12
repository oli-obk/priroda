mod graphviz;
mod locals;
mod source;

use rocket::response::content::Html;
use horrorshow::Template;
use PrirodaContext;
use step::Breakpoint;

use miri::{
    Frame,
    MemoryPointer,
    AllocId,
};

use rustc::hir::map::definitions::DefPathData;

use std::cell::Cell;

thread_local! {
    static FLASH_MESSAGE: Cell<String> = Cell::new(String::new());
}

pub fn set_flash_message(msg: String) {
    FLASH_MESSAGE.with(|key| key.set(msg));
}

pub fn render_main_window(
    pcx: &PrirodaContext,
    display_frame: Option<usize>,
) -> Html<String> {
    let message = FLASH_MESSAGE.with(|key| key.replace(String::new()));
    let is_active_stack_frame = match display_frame {
        Some(n) => n == pcx.stack().len() - 1,
        None => true,
    };
    let frame = display_frame.and_then(|frame| pcx.stack().get(frame)).or_else(|| pcx.stack().last());
    let filename = pcx.tcx.sess.local_crate_source_file.clone().unwrap_or_else(|| "no file name".to_string().into());
    let stack: Vec<(String, String, String)> = pcx.stack().iter().map(|&Frame { instance, span, .. } | {
        (
            if pcx.tcx.def_key(instance.def_id()).disambiguated_data.data == DefPathData::ClosureExpr {
                "inside call to closure".to_string()
            } else {
                instance.to_string()
            },
            pcx.tcx.sess.codemap().span_to_string(span),
            format!("{:?}", instance.def_id()),
        )
    }).collect();
    let rendered_breakpoints: Vec<String> = pcx.bptree.iter().map(|&Breakpoint(def_id, bb, stmt)| format!("{:?}@{}:{}", def_id, bb.index(), stmt)).collect();
    use rustc_data_structures::indexed_vec::Idx;
    let rendered_locals = locals::render_locals(pcx, frame);
    let rendered_source = source::render_source(pcx.tcx.tcx, frame);

    let mir_graph = frame.map(|frame| {
        graphviz::render_html(frame, pcx.bptree.for_def_id(frame.instance.def_id()))
    });

    println!("running horrorshow");
    use horrorshow::Raw;
    let mut buf = String::new();
    (html! {
        html {
            head {
                title { : filename.to_str().unwrap() }
                meta(charset = "UTF-8") {}
                script(src="/resources/svg-pan-zoom.js") {}
                script(src="/resources/zoom_mir.js") {}
            }
            body(onload="enable_mir_mousewheel()") {
                link(rel="stylesheet", href="/resources/style.css");
                link(rel="stylesheet", href="/resources/positioning.css");
                div(id="left") {
                    div(id="commands") {
                        @ if is_active_stack_frame {
                            a(href="/step/single") { div(title="Execute next MIR statement/terminator") { : "Step" } }
                            a(href="/step/next") { div(title="Run until after the next MIR statement/terminator") { : "Next" } }
                            a(href="/step/return") { div(title="Run until the function returns") { : "Return" } }
                            a(href="/step/single_back") { div(title="Execute previous MIR statement/terminator (restarts and steps till one stmt before the current stmt)") { : "Step back (slow)" } }
                            a(href="/step/continue") { div(title="Run until termination or breakpoint") { : "Continue" } }
                            a(href="/step/restart") { div(title="Abort execution and restart") { : "Restart" } }
                            a(href="/breakpoints/add_here") { div(title="Add breakpoint at current location") { : "Add breakpoint here"} }
                            a(href="/breakpoints/remove_all") { div(title="Remove all breakpoints") { : "Remove all breakpoints"} }
                        } else {
                            a(href="/") { div(title="Go to active stack frame") { : "Go back to active stack frame" } }
                        }
                    }
                    div(id="messages") {
                        p { : message }
                    }
                    div(id="mir") {
                        : Raw(mir_graph.unwrap_or("no current function".to_string()))
                    }
                }
                div(id="right") {
                    div(id="stack") {
                        table(border="1") {
                            @ for (i, &(ref s, ref span, ref def_id)) in stack.iter().enumerate().rev() {
                                tr {
                                    @ if i == display_frame.unwrap_or(stack.len() - 1) { td { : Raw("&#8594;") } } else { td; }
                                    td { : s }
                                    td { : span }
                                    td { : def_id }
                                    @ if i == display_frame.unwrap_or(stack.len() - 1) { td; } else { td { a(href=format!("/frame/{}", i)) { : "View" } } }
                                }
                            }
                        }
                    }
                    div(id="breakpoints") {
                        : "Breakpoints: "; br;
                        table(border="1") {
                            @ for bp in rendered_breakpoints.iter() {
                                tr {
                                    td { : bp }
                                    td { a(href=format!("/breakpoints/remove/{}", bp)) { : "remove" } }
                                }
                            }
                        }
                    }
                    div(id="locals") {
                        : Raw(rendered_locals)
                    }
                    div(id="source") {
                        : rendered_source
                    }
                }
            }
        }
    }).write_to_string(&mut buf).unwrap();
    Html(buf)
}

pub fn render_reverse_ptr<ERR: ::std::fmt::Debug>(
    pcx: &PrirodaContext,
    alloc_id: Option<Result<u64, ERR>>,
) -> Html<String> {
    let mut buf = String::new();
    match alloc_id {
        Some(Err(e)) => {
            set_flash_message(format!("not a number: {:?}", e));
            render_main_window(pcx, None)
        },
        Some(Ok(alloc_id)) => {
            let allocs: Vec<_> = pcx.memory().allocations().filter_map(|(id, alloc)| {
                alloc.relocations
                        .values()
                        .find(|reloc| reloc.0 == alloc_id)
                        .map(|_| id)
            }).collect();
            (html!{ html {
                head {
                    title { : format!("Allocations with pointers to Allocation {}", alloc_id) }
                    meta(charset = "UTF-8") {}
                }
                body {
                    @for id in allocs {
                        a(href=format!("/ptr/{}", id.0)) { : format!("Allocation {}", id) }
                        br;
                    }
                }
            }}).write_to_string(&mut buf).unwrap();
            Html(buf)
        },
        None => {
            set_flash_message("no allocation selected".to_string());
            render_main_window(pcx, None)
        }
    }
}

pub fn render_ptr_memory<ERR: ::std::fmt::Debug>(
    pcx: &PrirodaContext,
    alloc_id: Option<Result<AllocId, ERR>>,
    offset: Option<Result<u64, ERR>>,
) -> Html<String> {
    use horrorshow::Raw;
    let mut buf = String::new();
    match (alloc_id, offset) {
        (Some(Err(e)), _) |
        (_, Some(Err(e))) => {
            set_flash_message(format!("not a number: {:?}", e));
            render_main_window(pcx, None)
        }
        (Some(Ok(alloc_id)), offset) => {
            let offset = offset.unwrap_or(Ok(0)).expect("already checked in previous arm");
            let (mem, offset, rest) =
                if let Ok((_, mem, bytes)) = locals::print_ptr(&pcx.ecx, MemoryPointer {
                    alloc_id,
                    offset,
            }.into()) {
                if bytes * 2 > offset {
                    (mem, offset, (bytes * 2 - offset - 1) as usize)
                } else {
                    ("out of bounds offset".to_string(), 0, 0)
                }
            } else {
                ("unknown memory".to_string(), 0, 0)
            };
            (html!{ html {
                head {
                    title { : format!("Allocation {}", alloc_id) }
                    meta(charset = "UTF-8") {}
                }
                body {
                    span(style="font-family: monospace") {
                        : format!("{nil:.<offset$}┌{nil:─<rest$}", nil = "", offset = offset as usize, rest = rest)
                    }
                    br;
                    span(style="font-family: monospace") { : Raw(mem) }
                    br;
                    a(href=format!("/reverse_ptr/{}", alloc_id)) { : "List allocations with pointers into this allocation" }
                }
            }}).write_to_string(&mut buf).unwrap();
            Html(buf)
        },
        (None, _) => {
            set_flash_message("no allocation selected".to_string());
            render_main_window(pcx, None)
        }
    }
}

pub mod routes {
    use ::*;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![
            index,
            frame, frame_invalid,
            ptr,
            reverse_ptr,
        ]
    }

    #[get("/")]
    fn index(flash: Option<rocket::request::FlashMessage>, sender: State<PrirodaSender>) -> RResult<Html<String>> {
        sender.do_work(|pcx| {
            if let Some(flash) = flash {
                render::set_flash_message(flash.msg().to_string());
            }
            render::render_main_window(pcx, None)
        })
    }

    #[get("/frame/<frame>")]
    fn frame(sender: State<PrirodaSender>, frame: usize) -> RResult<Html<String>> {
        sender.do_work(move |pcx| {
            render::render_main_window(pcx, Some(frame))
        })
    }

    #[get("/frame/<frame>", rank = 42)] // Error handler
    fn frame_invalid(frame: String) -> BadRequest<String> {
        BadRequest(Some(format!("not a number: {:?}", frame.parse::<usize>().unwrap_err())))
    }

    #[get("/ptr/<path..>")]
    fn ptr(path: PathBuf, sender: State<PrirodaSender>) -> RResult<Html<String>> {
        sender.do_work(move |pcx| {
            let path = path.to_string_lossy();
            let mut matches = path.split('/');
            render::render_ptr_memory(pcx, matches.next().map(|id|Ok(AllocId(id.parse::<u64>()?))), matches.next().map(str::parse))
        })
    }

    #[get("/reverse_ptr/<ptr>")]
    fn reverse_ptr(ptr: String, sender: State<PrirodaSender>) -> RResult<Html<String>> {
        sender.do_work(move |pcx| {
            render::render_reverse_ptr(pcx, Some(ptr.parse()))
        })
    }
}
