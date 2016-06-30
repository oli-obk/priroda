use future::Promise;
use super::Page;
use super::Page::*;

use miri::{
    EvalContext,
    Frame,
    Pointer,
};

use rustc::session::Session;
use rustc::ty::TyCtxt;
use rustc::mir::repr as mir;

use std::borrow::Cow;

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
        let filename = session.local_crate_source_file.as_ref().map_or("no file name", |path| path.to_str().unwrap()).to_string();
        let stack: Vec<(String, String)> = ecx.stack().iter().map(|&Frame { def_id, span, .. } | {
            (tcx.item_path_str(def_id), session.codemap().span_to_string(span))
        }).collect();
        let print_local = |ty, ptr| {
            let ty = frame.map_or(ty, |frame| ecx.monomorphize(ty, frame.substs));
            let (alloc, text, len) = print_ptr(ecx, ptr);
            (ty.to_string(), alloc, text, len)
        };
        use rustc_data_structures::indexed_vec::Idx;
        let fn_args: Vec<(String, u64, String, usize)> = frame.map_or(Vec::new(), |&Frame { ref locals, var_offset, ref mir, .. }| {
            locals[..var_offset].iter().enumerate().map(|(id, &ptr)| print_local(mir.arg_decls[mir::Arg::new(id)].ty, ptr)).collect()
        });
        let fn_vars: Vec<(String, u64, String, usize)> = frame.map_or(Vec::new(), |&Frame { ref locals, var_offset, temp_offset, ref mir, .. }| {
            locals[var_offset..temp_offset].iter().enumerate().map(|(id, &ptr)| print_local(mir.var_decls[mir::Var::new(id)].ty, ptr)).collect()
        });
        let fn_temps: Vec<(String, u64, String, usize)> = frame.map_or(Vec::new(), |&Frame { ref locals, temp_offset, ref mir, .. }| {
            locals[temp_offset..].iter().enumerate().map(|(id, &ptr)| print_local(mir.temp_decls[mir::Temp::new(id)].ty, ptr)).collect()
        });
        let return_ptr = frame.and_then(|frame| frame.mir.return_ty.maybe_converging().map(|ty| print_local(ty, frame.return_ptr.unwrap())));

        let mir_graph = frame.map(|frame| {
            let mut mir_graphviz = String::new();
            super::graphviz::write(&frame.mir, &mut mir_graphviz).unwrap();
            String::from_utf8(::cgraph::Graph::from(mir_graphviz).render_dot().unwrap()).unwrap()
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
        promise.resolve(Html(box_html! {
            html {
                head {
                    title { : filename }
                }
                body {
                    style {
                        : raw!(include_str!("../style.css"))
                    }
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
                    div(id="stack") {
                        table(border="1") {
                            @ for (i, &(ref s, ref span)) in stack.iter().enumerate().rev() {
                                tr {
                                    @ if i == display_frame.unwrap_or(stack.len() - 1) { td { : "→" } } else { td; }
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
                                th { : "type" }
                                th { : "memory" }
                            }
                            @ if let Some((ty, alloc, text, _)) = return_ptr { tr {
                                td(colspan=2) { : "Return" }
                                td { : alloc.to_string() }
                                td { : ty }
                                td { : raw!(text) }
                            } }
                            @ for (i, &(ref ty, alloc, ref text, _)) in fn_args.iter().enumerate() {
                                tr {
                                    @if i == 0 { th(rowspan=fn_args.len().to_string()) { span(class="vertical") { : "Arguments" } } }
                                    td { : format!("arg{}", i) }
                                    td { : alloc.to_string() }
                                    td { : ty }
                                    td { : raw!(text) }
                                }
                            }
                            @ for (i, &(ref ty, alloc, ref text, _)) in fn_vars.iter().enumerate() {
                                tr {
                                    @if i == 0 { th(rowspan=fn_vars.len().to_string()) { span(class="vertical") { : "Variables" } } }
                                    td { : format!("var{}", i) }
                                    td { : alloc.to_string() }
                                    td { : ty }
                                    td { : raw!(text) }
                                }
                            }
                            @ for (i, &(ref ty, alloc, ref text, _)) in fn_temps.iter().enumerate() {
                                tr {
                                    @if i == 0 { th(rowspan=fn_temps.len().to_string()) { span(class="vertical") { : "Temporaries" } } }
                                    td { : format!("tmp{}", i) }
                                    td { : alloc.to_string() }
                                    td { : ty }
                                    td { : raw!(text) }
                                }
                            }
                        }
                    }
                    div(id="mir") {
                        style { : raw!{format!("
                            #node{} > text:nth-child({}) {{
                              fill: red;
                            }}
                        ", bb, stmt)}}
                        : raw!(mir_graph.unwrap_or("no current function".to_string()))
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
                let allocs: Vec<_> = self.ecx.memory().allocations().filter_map(|(&id, alloc)| {
                    for &reloc in alloc.relocations.values() {
                        if reloc.0 == alloc_id {
                            return Some(id)
                        }
                    }
                    None
                }).collect();
                self.promise.resolve(Html(box_html!{ html {
                    head {
                        title { : format!("Allocations with pointers to Allocation {}", alloc_id) }
                        body {
                            @for id in allocs {
                                a(href=format!("/ptr/{}", id)) { : format!("Allocation {}", id) }
                                br;
                            }
                        }
                    }
                }}));
            },
            None => self.render_main_window(None, "no allocation selected".to_string()),
        }
    }

    pub fn render_ptr_memory<ERR: ::std::fmt::Debug>(
        self,
        alloc_id: Option<Result<u64, ERR>>,
        offset: Option<Result<usize, ERR>>,
    ) {
        match (alloc_id, offset) {
            (Some(Err(e)), _) |
            (_, Some(Err(e))) => self.render_main_window(None, format!("not a number: {:?}", e)),
            (Some(Ok(alloc_id)), offset) => {
                let offset = offset.unwrap_or(Ok(0)).expect("already checked in previous arm");
                let (_, mem, bytes) = print_ptr(&self.ecx, Pointer {
                    alloc_id: ::miri::AllocId(alloc_id),
                    offset: offset,
                });
                self.promise.resolve(Html(box_html!{ html {
                    head {
                        title { : format!("Allocation {}", alloc_id) }
                        body {
                            span(style="font-family: monospace") {
                                : format!("{nil:.<offset$}┌{nil:─<rest$}", nil = "", offset = offset, rest = bytes * 2 - offset - 1)
                            }
                            br;
                            span(style="font-family: monospace") { : raw!(mem) }
                            br;
                            a(href=format!("/reverse_ptr/{}", alloc_id)) { : "List allocations with pointers into this allocation" }
                        }
                    }
                }}));
            },
            (None, _) => self.render_main_window(None, "no allocation selected".to_string()),
        }
    }
}

fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> (u64, String, usize) {
    match (ecx.memory().get(ptr.alloc_id), ecx.memory().get_fn(ptr.alloc_id)) {
        (Ok(alloc), Err(_)) => {
            use std::fmt::Write;
            let mut s = String::new();
            let mut i = 0;
            while i < alloc.bytes.len() {
                if let Some(&reloc) = alloc.relocations.get(&i) {
                    i += ecx.memory().pointer_size();
                    write!(&mut s,
                        "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                        alloc = reloc,
                        offset = ptr.offset,
                        nil = "",
                        wdt = ecx.memory().pointer_size() * 2 - 2,
                    ).unwrap();
                } else {
                    if alloc.undef_mask.is_range_defined(i, i + 1) {
                        write!(&mut s, "{:02x}", alloc.bytes[i]).unwrap();
                    } else {
                        let ub_chars = ['∅','∆','∇','∓','∞','⊙','⊠','⊘','⊗','⊛','⊝','⊡','⊠'];
                        let c1 = (ptr.alloc_id.0 * 769 + i as u64 * 5689) as usize % ub_chars.len();
                        let c2 = (ptr.alloc_id.0 * 997 + i as u64 * 7193) as usize % ub_chars.len();
                        write!(&mut s, "<mark>{}{}</mark>", ub_chars[c1], ub_chars[c2]).unwrap();
                    }
                    i += 1;
                }
            }
            (ptr.alloc_id.0, s, alloc.bytes.len())
        },
        (Err(_), Ok(_)) => {
            // FIXME: print function name
            (ptr.alloc_id.0, "function pointer".to_string(), 0)
        },
        (Err(_), Err(_)) => unreachable!(),
        (Ok(_), Ok(_)) => unreachable!(),
    }
}
