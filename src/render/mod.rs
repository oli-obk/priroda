mod graphviz;
pub mod locals;
mod source;

use rustc::hir::map::definitions::DefPathData;
use rustc::ty::layout::Size;

use rocket::response::content::Html;
use horrorshow::{Raw, Template};

use miri::{
    Frame,
    Pointer,
    AllocId,
};

use PrirodaContext;
use step::Breakpoint;

pub fn template(pcx: &PrirodaContext, title: String, t: impl Template) -> Html<String> {
    let mut buf = String::new();
    (html! {
        html {
            head {
                title { : title }
                meta(charset = "UTF-8") {}
                script(src="/resources/svg-pan-zoom.js") {}
                script(src="/resources/zoom_mir.js") {}
                : Raw(refresh_script(pcx))
            }
            body(onload="enable_mir_mousewheel()") {
                link(rel="stylesheet", href="/resources/positioning.css");
                link(rel="stylesheet", href=format!("/resources/style-{}.css", pcx.config.theme));
                : t
            }
        }
    }).write_to_string(&mut buf).unwrap();
    Html(buf)
}

pub fn refresh_script(pcx: &PrirodaContext) -> String {
    if pcx.config.auto_refresh {
        r#"<script>
            setInterval(() => {
                fetch("/step_count").then((res) => {
                    if(res.status == 200) {
                        return res.text();
                    } else {
                        throw "";
                    }
                }).then((res) => {
                    if(res != #step_count#) {
                        window.location.reload();
                    }
                }).catch(()=>{});
            }, 1000);
        </script>"#.replace("#step_count#", &format!("{}", pcx.step_count))
    } else {
        String::new()
    }
}

pub fn render_main_window(
    pcx: &PrirodaContext,
    display_frame: Option<usize>,
    message: String,
) -> Html<String> {
    let is_active_stack_frame = match display_frame {
        Some(n) => n == pcx.ecx.stack().len() - 1,
        None => true,
    };
    let frame = display_frame.and_then(|frame| pcx.ecx.stack().get(frame)).or_else(|| pcx.ecx.stack().last());
    let stack: Vec<(String, String, String)> = pcx.ecx.stack().iter().map(|&Frame { instance, span, .. } | {
        (
            if pcx.ecx.tcx.def_key(instance.def_id()).disambiguated_data.data == DefPathData::ClosureExpr {
                "inside call to closure".to_string()
            } else {
                instance.to_string()
            },
            pcx.ecx.tcx.sess.codemap().span_to_string(span),
            format!("{:?}", instance.def_id()),
        )
    }).collect();
    let rendered_breakpoints: Vec<String> = pcx.config.bptree.iter().map(|&Breakpoint(def_id, bb, stmt)| format!("{:?}@{}:{}", def_id, bb.index(), stmt)).collect();
    use rustc_data_structures::indexed_vec::Idx;
    let rendered_locals = locals::render_locals(&pcx.ecx, frame);
    let rendered_source = source::render_source(pcx.ecx.tcx.tcx, frame);

    let mir_graph = frame.map(|frame| {
        graphviz::render_html(frame, pcx.config.bptree.for_def_id(frame.instance.def_id()))
    });

    let filename = pcx.ecx.tcx.sess.local_crate_source_file.as_ref().map(|f| f.display().to_string()).unwrap_or_else(|| "no file name".to_string());
    template(pcx, filename, html! {
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
            div {
                : format!("Step count: {}", pcx.step_count);
            }
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
    })
}

pub fn render_reverse_ptr<ERR: ::std::fmt::Debug>(
    pcx: &PrirodaContext,
    alloc_id: Option<Result<u64, ERR>>,
) -> Html<String> {
    match alloc_id {
        Some(Err(e)) => {
            render_main_window(pcx, None, format!("not a number: {:?}", e))
        },
        Some(Ok(alloc_id)) => {
            let allocs: Vec<_> = pcx.ecx.memory().allocations().filter_map(|(id, alloc)| {
                alloc.relocations
                        .values()
                        .find(|reloc| reloc.0 == alloc_id)
                        .map(|_| id)
            }).collect();
            template(pcx, format!("Allocations with pointers to Allocation {}", alloc_id), html!{
                @for id in allocs {
                    a(href=format!("/ptr/{}", id.0)) { : format!("Allocation {}", id) }
                    br;
                }
            })
        },
        None => {
            render_main_window(pcx, None, "no allocation selected".to_string())
        }
    }
}

pub fn render_ptr_memory<ERR: ::std::fmt::Debug>(
    pcx: &PrirodaContext,
    alloc_id: Option<Result<AllocId, ERR>>,
    offset: Option<Result<u64, ERR>>,
) -> Html<String> {
    use horrorshow::Raw;
    match (alloc_id, offset) {
        (Some(Err(e)), _) |
        (_, Some(Err(e))) => {
            render_main_window(pcx, None, format!("not a number: {:?}", e))
        }
        (Some(Ok(alloc_id)), offset) => {
            let offset = offset.unwrap_or(Ok(0)).expect("already checked in previous arm");
            let (mem, offset, rest) =
                if let Ok((_, mem, bytes)) = locals::print_ptr(&pcx.ecx, Pointer {
                    alloc_id,
                    offset: Size::from_bytes(offset),
            }.into()) {
                if bytes * 2 > offset {
                    (mem, offset, (bytes * 2 - offset - 1) as usize)
                } else if bytes * 2 == 0 && offset == 0 {
                    (mem, 0, 0)
                } else {
                    ("out of bounds offset".to_string(), 0, 0)
                }
            } else {
                ("unknown memory".to_string(), 0, 0)
            };
            template(pcx, format!("Allocation {}", alloc_id), html!{
                span(style="font-family: monospace") {
                    : format!("{nil:.<offset$}┌{nil:─<rest$}", nil = "", offset = offset as usize, rest = rest)
                }
                br;
                span(style="font-family: monospace") { : Raw(mem) }
                br;
                a(href=format!("/reverse_ptr/{}", alloc_id)) { : "List allocations with pointers into this allocation" }
            })
        },
        (None, _) => {
            render_main_window(pcx, None, "no allocation selected".to_string())
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
            let flash = flash.map(|flash| flash.msg().to_string()).unwrap_or_else(String::new);
            render::render_main_window(pcx, None, flash)
        })
    }

    #[get("/frame/<frame>")]
    fn frame(flash: Option<rocket::request::FlashMessage>, sender: State<PrirodaSender>, frame: usize) -> RResult<Html<String>> {
        sender.do_work(move |pcx| {
            let flash = flash.map(|flash| flash.msg().to_string()).unwrap_or_else(String::new);
            render::render_main_window(pcx, Some(frame), flash)
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
