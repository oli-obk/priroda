use promising_future::Promise;
use super::Page;
use super::Page::*;

use miri::{
    EvalContext,
    Frame,
    Pointer,
    Value,
    PrimVal,
};

use rustc::session::Session;
use rustc::ty::TyCtxt;
use rustc::mir::repr as mir;

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
        let filename = session.local_crate_source_file.as_ref().map_or("no file name", |path| path.to_str().unwrap()).to_string();
        let stack: Vec<(String, String)> = ecx.stack().iter().map(|&Frame { def_id, span, .. } | {
            (tcx.item_path_str(def_id), session.codemap().span_to_string(span))
        }).collect();
        use rustc_data_structures::indexed_vec::Idx;
        let locals: Vec<(String, Option<u64>, String, usize)> = frame.map_or(Vec::new(), |&Frame { ref locals, ref mir, ref return_lvalue, ref substs, .. }| {
            let ret_val = ecx.read_lvalue(*return_lvalue).ok();
            iter::once(&ret_val).chain(locals.iter()).enumerate().map(|(id, &val)| {
                let ty = mir.local_decls[mir::Local::new(id)].ty;
                let ty = ecx.monomorphize(ty, substs);
                match val.map(|value| print_value(ecx, value)) {
                    Some(Ok((alloc, text, len))) => (ty.to_string(), alloc, text, len),
                    Some(Err(())) => (ty.to_string(), None, format!("{:?} does not exist", val), 0),
                    None => (ty.to_string(), None, "uninitialized".to_owned(), 0),
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
        promise.set(Html(box_html! {
            html {
                head {
                    title { : filename }
                    script(type="text/javascript") { : raw!(include_str!("../svg-pan-zoom/dist/svg-pan-zoom.js")) }
                    script(type="text/javascript") { : raw!(include_str!("../zoom_mir.js")) }
                }
                body(onload="enable_mir_mousewheel()") {
                    style { : raw!(include_str!("../style.css")) }
                    style { : raw!(include_str!("../positioning.css")) }
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
                                th { : "memory" }
                                th { : "type" }
                            }
                            @ for (i, &(ref ty, alloc, ref text, _)) in locals.iter().enumerate() {
                                tr {
                                    @if i == 0 {
                                        th(colspan = 2) { : "Return" }
                                    } else if i == 1 && arg_count != 0 {
                                        th(rowspan=arg_count) { span(class="vertical") { : "Arguments" } }
                                    } else if i == arg_count + 1 {
                                        th(rowspan=var_count) { span(class="vertical") { : "Variables" } }
                                    } else if i == var_count + arg_count + 1 {
                                        th(rowspan=tmp_count) { span(class="vertical") { : "Temporaries" } }
                                    }
                                    @if i != 0 {
                                        td { : format!("_{}", i) }
                                    }
                                    @if let Some(alloc) = alloc {
                                        td { : alloc.to_string() }
                                    } else {
                                        td;
                                    }
                                    td { : raw!(text) }
                                    td { : ty }
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
                    alloc.relocations
                         .values()
                         .find(|reloc| reloc.0 == alloc_id)
                         .map(|_| id)
                }).collect();
                self.promise.set(Html(box_html!{ html {
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
                }).unwrap_or((None, "unknown memory".to_string(), 0));
                self.promise.set(Html(box_html!{ html {
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

fn print_primval(val: PrimVal) -> String {
    use miri::PrimValKind::*;
    match val.kind {
        Ptr => format!("<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>", alloc = val.relocation.unwrap(), offset = val.bits),
        FnPtr => "function pointer".to_string(),
        U8 | U16 | U32 | U64 => val.bits.to_string(),
        I8 | I16 | I32 | I64 => (val.bits as i64).to_string(),
        F32 => val.to_f32().to_string(),
        F64 => val.to_f64().to_string(),
        Bool => val.try_as_bool().expect("bad bool primval").to_string(),
        Char => ::std::char::from_u32(val.bits as u32).expect("bad char primval").to_string(),
    }
}

fn print_value(ecx: &EvalContext, val: Value) -> Result<(Option<u64>, String, usize), ()> {
    let txt = match val {
        Value::ByRef(ptr) => return print_ptr(ecx, ptr),
        Value::ByVal(primval) => print_primval(primval),
        Value::ByValPair(val, extra) => format!("{}, {}", print_primval(val), print_primval(extra)),
    };
    Ok((None, txt, 0))
}

fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> Result<(Option<u64>, String, usize), ()> {
    if ptr.points_to_zst() || ptr == Pointer::never_ptr() {
        return Ok((None, String::new(), 0));
    }
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
            Ok((Some(ptr.alloc_id.0), s, alloc.bytes.len()))
        },
        (Err(_), Ok(_)) => {
            // FIXME: print function name
            Ok((None, "function pointer".to_string(), 16))
        },
        (Err(_), Err(_)) => Err(()),
        (Ok(_), Ok(_)) => unreachable!(),
    }
}
