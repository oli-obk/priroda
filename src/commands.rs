use promising_future::Promise;
use super::{Page, EvalContext};
use super::Page::*;

use miri::{
    Frame,
    MemoryPointer,
    Value,
    PrimVal,
    Pointer,
    AllocId,
};


use rustc::hir::map::definitions::DefPathData;
use rustc::session::Session;
use rustc::ty::TyCtxt;
use rustc::mir;

use std::borrow::Cow;
use std::iter;

pub(crate) struct Renderer<'a, 'tcx: 'a> {
    pub promise: Promise<Page>,
    pub ecx: &'a EvalContext<'a, 'tcx>,
    pub tcx: TyCtxt<'a, 'tcx, 'tcx>,
    pub session: &'a Session,
}

impl<'a, 'tcx: 'a> Renderer<'a, 'tcx> {
    pub fn new(
        promise: Promise<Page>,
        ecx: &'a EvalContext<'a, 'tcx>,
        tcx: TyCtxt<'a, 'tcx, 'tcx>,
        session: &'a Session,
    ) -> Self {
        Renderer {
            promise: promise,
            ecx: ecx,
            tcx: tcx,
            session: session,
        }
    }
    pub fn render_main_window<MSG: Into<Cow<'static, str>>>(
        self,
        display_frame: Option<usize>,
        message: MSG,
    ) {
        let Renderer { promise, ecx, tcx, session } = self;
        let message = message.into().into_owned();
        let is_active_stack_frame = match display_frame {
            Some(n) => n == ecx.stack().len() - 1,
            None => true,
        };
        let frame = display_frame.and_then(|frame| ecx.stack().get(frame)).or_else(|| ecx.stack().last());
        let filename = session.local_crate_source_file.clone().unwrap_or_else(|| "no file name".to_string().into());
        let stack: Vec<(String, String)> = ecx.stack().iter().map(|&Frame { instance, span, .. } | {
            (
                if tcx.def_key(instance.def_id()).disambiguated_data.data == DefPathData::ClosureExpr {
                    "inside call to closure".to_string()
                } else {
                    instance.to_string()
                },
                session.codemap().span_to_string(span),
            )
        }).collect();
        use rustc_data_structures::indexed_vec::Idx;
        let locals: Vec<(String, Option<u64>, String, u64)> = frame.map_or(Vec::new(), |&Frame { instance, ref locals, ref mir, ref return_place, .. }| {
            let ret_val = ecx.read_place(*return_place).ok();
            iter::once(&ret_val).chain(locals.iter()).enumerate().map(|(id, &val)| {
                let ty = mir.local_decls[mir::Local::new(id)].ty;
                let ty = ecx.monomorphize(ty, instance.substs);
                match val.map(|value| print_value(ecx, value)) {
                    Some(Ok((alloc, text, len))) => (ty.to_string(), alloc, text, len),
                    Some(Err(())) => (ty.to_string(), None, format!("{:?} does not exist", val), 0),
                    None => (ty.to_string(), None, "&lt;uninit&gt;".to_owned(), 0),
                }
            }).collect()
        });
        let (arg_count, var_count, tmp_count) = frame.map_or((0, 0, 0), |&Frame { ref mir, .. }| (
            mir.args_iter().count(),
            mir.vars_iter().count(),
            mir.temps_iter().count(),
        ));

        let mir_graph = frame.map(|frame| {
            let mut mir_graphviz = String::new();
            super::graphviz::write(&frame.mir, &mut mir_graphviz).unwrap();
            String::from_utf8(::cgraph::Graph::parse(mir_graphviz).unwrap().render_dot().unwrap()).unwrap()
        });
        let (bb, stmt) = frame.map_or((0, 0), |frame| {
            let blck = &frame.mir.basic_blocks()[frame.block];
            use rustc_data_structures::indexed_vec::Idx;
            (
                frame.block.index() + 1,
                if frame.stmt == blck.statements.len() {
                    if blck.statements.is_empty() {
                        6
                    } else {
                        blck.statements.len() + 7
                    }
                } else {
                    assert!(frame.stmt < blck.statements.len());
                    frame.stmt + 6
                },
            )
        });
        let edge_colors = if let Some(ref frame) = frame {
            let blck = &frame.mir.basic_blocks()[frame.block];
            let (targets, unwind) = if frame.stmt == blck.statements.len() {
                use rustc::mir::TerminatorKind::*;
                match blck.terminator().kind {
                    Goto { target } => (vec![target], None),
                    SwitchInt { ref targets, .. } => (targets.to_vec(), None),
                    Drop { target, unwind, .. } |
                    DropAndReplace { target, unwind, .. } => (vec![target], unwind),
                    Call { ref destination, cleanup, .. } => {
                        if let Some((_, target)) = *destination {
                            (vec![target], cleanup)
                        } else {
                            (vec![], cleanup)
                        }
                    }
                    _ => (vec![], None),
                }
            } else {
                (vec![], None)
            };
            format!("let edge_colors = {{{}}};",
                targets.into_iter().map(|target|(frame.block, target, "green"))
                    .chain(unwind.into_iter().map(|target|(frame.block, target, "red")))
                    .map(|(from, to, color)| format!("'bb{}->bb{}':'{}'", from.index(), to.index(), color))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        } else {
            "{}".to_string()
        };
        println!("running horrorshow");
        use horrorshow::Raw;
        promise.set(Html(box_html! {
            html {
                head {
                    title { : filename.to_str().unwrap() }
                    meta(charset = "UTF-8") {}
                    script(type="text/javascript") { : Raw(include_str!("../svg-pan-zoom/dist/svg-pan-zoom.js")) }
                    script(type="text/javascript") { : Raw(include_str!("../zoom_mir.js")) }
                }
                body(onload="enable_mir_mousewheel()") {
                    style { : Raw(include_str!("../style.css")) }
                    style { : Raw(include_str!("../positioning.css")) }
                    div(id="left") {
                        div(id="commands") {
                            @ if is_active_stack_frame {
                                a(href="/step") { div(title="Execute next MIR statement/terminator") { : "Step" } }
                                a(href="/next") { div(title="Run until after the next MIR statement/terminator") { : "Next" } }
                                a(href="/return") { div(title="Run until the function returns") { : "Return" } }
                                a(href="/continue") { div(title="Run until termination or breakpoint") { : "Continue" } }
                            } else {
                                a(href="/") { div(title="Go to active stack frame") { : "Go back to active stack frame" } }
                            }
                        }
                        div(id="messages") {
                            p { : message }
                        }
                        div(id="mir") {
                            style { : Raw(format!("
                                #node{} > text:nth-child({}) {{
                                fill: red;
                                }}
                                .edge-green > path, .edge-green > polygon, .edge-green > text {{
                                    fill: green;
                                    stroke: green;
                                }}
                                .edge-red > path, .edge-red > polygon, .edge-red > text {{
                                    fill: red;
                                    stroke: red;
                                }}
                                .edge > path {{
                                    fill: none;
                                }}
                            ", bb, stmt))}
                            : Raw(mir_graph.unwrap_or("no current function".to_string()))
                        }
                        script { : Raw(edge_colors + r##"
                                for(let el of document.querySelectorAll("#mir > svg #graph0 .edge")) {
                                    let title = el.querySelector("title").textContent;
                                    if(title in edge_colors) {
                                        el.classList.add("edge-" + edge_colors[title]);
                                    }
                                }"##) }
                    }
                    div(id="right") {
                        div(id="stack") {
                            table(border="1") {
                                @ for (i, &(ref s, ref span)) in stack.iter().enumerate().rev() {
                                    tr {
                                        @ if i == display_frame.unwrap_or(stack.len() - 1) { td { : Raw("&#8594;") } } else { td; }
                                        td { : s }
                                        td { : span }
                                        @ if i == display_frame.unwrap_or(stack.len() - 1) { td; } else { td { a(href=format!("/frame/{}", i)) { : "View" } } }
                                    }
                                }
                            }
                        }
                        div(id="locals") {
                            table(border="1") {
                                tr {
                                    td(width="20px");
                                    th { : "id" }
                                    th { : "alloc" }
                                    th { : "memory" }
                                    th { : "type" }
                                }
                                @ for (i, &(ref ty, alloc, ref text, _)) in locals.iter().enumerate() {
                                    tr {
                                        @if i == 0 {
                                            th(rowspan=1) { span(class="vertical") { : "Return" } }
                                        } else if i == 1 && arg_count != 0 {
                                            th(rowspan=arg_count) { span(class="vertical") { : "Arguments" } }
                                        } else if i == arg_count + 1 {
                                            th(rowspan=var_count) { span(class="vertical") { : "Variables" } }
                                        } else if i == var_count + arg_count + 1 {
                                            th(rowspan=tmp_count) { span(class="vertical") { : "Temporaries" } }
                                        }
                                        td { : format!("_{}", i) }
                                        @if let Some(alloc) = alloc {
                                            td { : alloc.to_string() }
                                        } else {
                                            td;
                                        }
                                        td { : Raw(text) }
                                        td { : ty }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }));
    }

    pub fn render_reverse_ptr<ERR: ::std::fmt::Debug>(
        self,
        alloc_id: Option<Result<u64, ERR>>,
    ) {
        match alloc_id {
            Some(Err(e)) => self.render_main_window(None, format!("not a number: {:?}", e)),
            Some(Ok(alloc_id)) => {
                let allocs: Vec<_> = self.ecx.memory().allocations().filter_map(|(id, alloc)| {
                    alloc.relocations
                         .values()
                         .find(|reloc| reloc.0 == alloc_id)
                         .map(|_| id)
                }).collect();
                self.promise.set(Html(box_html!{ html {
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
                }}));
            },
            None => self.render_main_window(None, "no allocation selected".to_string()),
        }
    }

    pub fn render_ptr_memory<ERR: ::std::fmt::Debug>(
        self,
        alloc_id: Option<Result<AllocId, ERR>>,
        offset: Option<Result<u64, ERR>>,
    ) {
        use horrorshow::Raw;
        match (alloc_id, offset) {
            (Some(Err(e)), _) |
            (_, Some(Err(e))) => self.render_main_window(None, format!("not a number: {:?}", e)),
            (Some(Ok(alloc_id)), offset) => {
                let offset = offset.unwrap_or(Ok(0)).expect("already checked in previous arm");
                let (_, mem, bytes) = print_ptr(&self.ecx, MemoryPointer {
                    alloc_id: alloc_id,
                    offset: offset,
                }.into()).unwrap_or((None, "unknown memory".to_string(), 0));
                self.promise.set(Html(box_html!{ html {
                    head {
                        title { : format!("Allocation {}", alloc_id) }
                        meta(charset = "UTF-8") {}
                    }
                    body {
                        span(style="font-family: monospace") {
                            : format!("{nil:.<offset$}┌{nil:─<rest$}", nil = "", offset = offset as usize, rest = (bytes * 2 - offset - 1) as usize)
                        }
                        br;
                        span(style="font-family: monospace") { : Raw(mem) }
                        br;
                        a(href=format!("/reverse_ptr/{}", alloc_id)) { : "List allocations with pointers into this allocation" }
                    }
                }}));
            },
            (None, _) => self.render_main_window(None, "no allocation selected".to_string()),
        }
    }
}

fn print_primval(val: PrimVal) -> String {
    match val {
        PrimVal::Undef => "&lt;undef &gt;".to_string(),
        PrimVal::Ptr(ptr) => format!("<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>", alloc = ptr.alloc_id.0, offset = ptr.offset),
        // FIXME: print prettier depending on type
        PrimVal::Bytes(bytes) => bytes.to_string(),
    }
}

fn print_value(ecx: &EvalContext, val: Value) -> Result<(Option<u64>, String, u64), ()> {
    let txt = match val {
        Value::ByRef(ptr, _align) => return print_ptr(ecx, ptr),
        Value::ByVal(primval) => print_primval(primval),
        Value::ByValPair(val, extra) => format!("{}, {}", print_primval(val), print_primval(extra)),
    };
    Ok((None, txt, 0))
}

fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> Result<(Option<u64>, String, u64), ()> {
    let ptr = ptr.to_ptr().map_err(|_| ())?;
    match (ecx.memory().get(ptr.alloc_id), ecx.memory().get_fn(ptr)) {
        (Ok(alloc), Err(_)) => {
            use std::fmt::Write;
            let mut s = String::new();
            let mut i = 0;
            while i < alloc.bytes.len() as u64 {
                if let Some(&reloc) = alloc.relocations.get(&i) {
                    i += ecx.memory().pointer_size();
                    write!(&mut s,
                        "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                        alloc = reloc.0,
                        offset = ptr.offset,
                        nil = "",
                        wdt = (ecx.memory().pointer_size() * 2 - 2) as usize,
                    ).unwrap();
                } else {
                    if alloc.undef_mask.is_range_defined(i, i + 1) {
                        write!(&mut s, "{:02x}", alloc.bytes[i as usize] as usize).unwrap();
                    } else {
                        let ub_chars = ['∅','∆','∇','∓','∞','⊙','⊠','⊘','⊗','⊛','⊝','⊡','⊠'];
                        let c1 = (ptr.alloc_id.0 * 769 + i as u64 * 5689) as usize % ub_chars.len();
                        let c2 = (ptr.alloc_id.0 * 997 + i as u64 * 7193) as usize % ub_chars.len();
                        write!(&mut s, "<mark>{}{}</mark>", ub_chars[c1], ub_chars[c2]).unwrap();
                    }
                    i += 1;
                }
            }
            Ok((Some(ptr.alloc_id.0), s, alloc.bytes.len() as u64))
        },
        (Err(_), Ok(_)) => {
            // FIXME: print function name
            Ok((None, "function pointer".to_string(), 16))
        },
        (Err(_), Err(_)) => Err(()),
        (Ok(_), Ok(_)) => unreachable!(),
    }
}
