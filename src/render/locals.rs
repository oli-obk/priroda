use rustc_middle::mir::{
    self,
    interpret::{InterpError, UndefinedBehaviorInfo},
};
use rustc_middle::ty::{subst::Subst, ParamEnv, TyKind, TyS, TypeAndMut};
use rustc_mir::interpret::{Frame, MemPlaceMeta, Scalar, ScalarMaybeUninit};
use rustc_target::abi::{Abi, Size};

use miri::{AllocExtra, Tag};

use horrorshow::prelude::*;
use horrorshow::Template;

use crate::InterpCx;

pub fn render_locals<'tcx>(
    ecx: &InterpCx<'tcx>,
    frame: &Frame<'tcx, 'tcx, Tag, miri::FrameData<'tcx>>,
) -> String {
    let &Frame {
        ref body,
        ref return_place,
        ref instance,
        ..
    } = frame;

    //               name    ty      alloc        val     style
    let locals: Vec<(String, String, Option<u64>, String, &str)> = body
        .local_decls
        .iter_enumerated()
        .map(|(id, local_decl)| {
            let name = body
                .var_debug_info
                .iter()
                .find(|var_debug_info| match var_debug_info.value {
                    mir::VarDebugInfoContents::Place(place) => {
                        place.projection.is_empty() && place.local == id
                    }
                    _ => false,
                })
                .map(|var_debug_info| var_debug_info.name.as_str().to_string())
                .unwrap_or_else(String::new);

            // FIXME Don't panic when trying to read from uninit variable.
            // Panic message:
            // > error: internal compiler error: src/librustc_mir/interpret/eval_context.rs:142:
            // > The type checker should prevent reading from a never-written local
            let op_ty = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                if id == mir::RETURN_PLACE {
                    return_place
                        .map(|p| ecx.place_to_op(&p).unwrap())
                        .ok_or(false)
                } else {
                    ecx.access_local(frame, id, None).map_err(|_| false)
                }
            })) {
                Ok(op_ty) => op_ty,
                Err(_) => Err(true),
            };

            let (alloc, val, style) = match op_ty {
                Err(false) => (None, "&lt;dead&gt;".to_owned(), "font-size: 0;"),
                Err(true) => (None, "&lt;uninit&gt;".to_owned(), "color: darkmagenta;"),
                Ok(op_ty) => match print_operand(ecx, op_ty) {
                    Ok((alloc, text)) => (alloc, text, ""),
                    Err(()) => (None, "&lt;error&gt;".to_owned(), "color: red;"),
                },
            };
            let ty = ecx.tcx.normalize_erasing_regions(
                ParamEnv::reveal_all(),
                local_decl.ty.subst(ecx.tcx.tcx, instance.substs),
            );
            (name, ty.to_string(), alloc, val, style)
        })
        .collect();

    let (arg_count, var_count, tmp_count) = (
        body.args_iter().count(),
        body.vars_iter().count(),
        body.temps_iter().count(),
    );

    (horrorshow::html! {
        table(border="1") {
            tr {
                td(width="20px");
                th { : "id" }
                th { : "name" }
                th { : "alloc" }
                th { : "memory" }
                th { : "type" }
            }
            @ for (i, &(ref name, ref ty, alloc, ref text, ref style)) in locals.iter().enumerate() {
                tr(style=style) {
                    @if i == 0 {
                        th(rowspan=1) { span(class="vertical") { : "Return" } }
                    } else if i == 1 && arg_count != 0 {
                        th(rowspan=arg_count) { span(class="vertical") { : "Arguments" } }
                    } else if i == arg_count + 1 && var_count != 0 {
                        th(rowspan=var_count) { span(class="vertical") { : "Variables" } }
                    } else if i == var_count + arg_count + 1 && tmp_count != 0 {
                        th(rowspan=tmp_count) { span(class="vertical") { : "Temporaries" } }
                    }
                    td { : format!("_{}", i) }
                    td { : name }
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
    }).into_string()
        .unwrap()
}

fn print_scalar_maybe_undef(val: ScalarMaybeUninit<miri::Tag>) -> String {
    match val {
        ScalarMaybeUninit::Uninit => "&lt;undef &gt;".to_string(),
        ScalarMaybeUninit::Scalar(val) => print_scalar(val),
    }
}

fn print_scalar(val: Scalar<miri::Tag>) -> String {
    match val {
        Scalar::Ptr(ptr) => format!(
            "<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>",
            alloc = ptr.alloc_id.0,
            offset = ptr.offset.bytes()
        ),
        Scalar::Raw { data, size } => {
            if size == 0 {
                "&lt;zst&gt;".to_string()
            } else {
                format!("0x{:0width$X}", data, width = (size as usize) / 8)
            }
        }
    }
}

fn pp_operand<'tcx>(
    ecx: &InterpCx<'tcx>,
    op_ty: OpTy<'tcx, miri::Tag>,
) -> InterpResult<'tcx, String> {
    let err = || InterpError::UndefinedBehavior(UndefinedBehaviorInfo::Ub(String::new()));
    match op_ty.layout.ty.kind {
        TyKind::RawPtr(TypeAndMut {
            ty: &TyS {
                kind: TyKind::Str, ..
            },
            ..
        })
        | TyKind::Ref(
            _,
            &TyS {
                kind: TyKind::Str, ..
            },
            _,
        ) => {
            if let Operand::Immediate(val) = *op_ty {
                if let Immediate::ScalarPair(
                    ScalarMaybeUninit::Scalar(Scalar::Ptr(ptr)),
                    ScalarMaybeUninit::Scalar(Scalar::Raw { data: len, .. }),
                ) = val
                {
                    if let Ok(allocation) = ecx.memory.get_raw(ptr.alloc_id) {
                        let offset = ptr.offset.bytes();
                        if (offset as u128) < allocation.len() as u128 {
                            let start = offset as usize;
                            let end = start.checked_add(len as usize).ok_or(err())?;
                            let alloc_bytes = &allocation
                                .inspect_with_undef_and_ptr_outside_interpreter(start..end);
                            let s = String::from_utf8_lossy(alloc_bytes);
                            return Ok(format!("\"{}\"", s));
                        }
                    }
                }
            }
        }
        TyKind::Adt(adt_def, _substs) => {
            if let Operand::Immediate(Immediate::Scalar(ScalarMaybeUninit::Uninit)) = *op_ty {
                Err(err())?;
            }

            let variant = ecx.read_discriminant(op_ty)?.1;
            let adt_fields = &adt_def.variants[variant].fields;

            let should_collapse = adt_fields.len() > 1;

            //println!("{:?} {:?} {:?}", val, ty, adt_def.variants);
            let mut pretty = ecx
                .tcx
                .def_path_str(adt_def.did)
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .to_string();

            if adt_def.is_enum() {
                pretty.push_str("::");
                pretty.push_str(&*adt_def.variants[variant].ident.as_str());
            }
            pretty.push_str(" { ");

            if should_collapse {
                pretty.push_str("<details>");
            }

            for (i, adt_field) in adt_fields.iter().enumerate() {
                let field_pretty: InterpResult<String> = try {
                    let field_op_ty = ecx.operand_field(op_ty, i as usize)?;
                    pp_operand(ecx, field_op_ty)?
                };

                pretty.push_str(&format!(
                    "{}: {}, ",
                    adt_field.ident.as_str(),
                    match field_pretty {
                        Ok(field_pretty) => field_pretty,
                        Err(_err) => "<span style='color: red;'>&lt;err&gt;</span>".to_string(),
                    }
                ));
                if should_collapse {
                    pretty.push_str("<br>");
                }
            }

            if should_collapse {
                pretty.push_str("</details>");
            }

            pretty.push_str("}");
            println!("pretty adt: {}", pretty);
            return Ok(pretty);
        }
        _ => {}
    }

    if op_ty.layout.size.bytes() == 0 {
        Err(err())?;
    }
    if let Abi::Scalar(_) = op_ty.layout.abi {
    } else {
        Err(err())?;
    }
    let scalar = ecx.read_scalar(op_ty)?;
    if let ScalarMaybeUninit::Scalar(Scalar::Ptr(_)) = &scalar {
        return Ok(print_scalar_maybe_undef(scalar)); // If the value is a ptr, print it
    }
    match op_ty.layout.ty.kind {
        TyKind::Bool => {
            if scalar.to_bool()? {
                Ok("true".to_string())
            } else {
                Ok("false".to_string())
            }
        }
        TyKind::Char => {
            let chr = scalar.to_char()?;
            if chr.is_ascii() {
                Ok(format!("'{}'", chr))
            } else {
                Err(err().into())
            }
        }
        TyKind::Uint(_) => Ok(format!("{0}", scalar.to_u64()?)),
        TyKind::Int(_) => Ok(format!("{0}", scalar.to_i64()?)),
        TyKind::Float(float_ty) => {
            use rustc_ast::ast::FloatTy::*;
            match float_ty {
                F32 => Ok(format!("{}", scalar.to_f32()?)),
                F64 => Ok(format!("{}", scalar.to_f64()?)),
            }
        }
        _ => Err(err().into()),
    }
}

pub fn print_operand<'a, 'tcx: 'a>(
    ecx: &InterpCx<'tcx>,
    op_ty: OpTy<'tcx, miri::Tag>,
) -> Result<(Option<u64>, String), ()> {
    let pretty = pp_operand(ecx, op_ty);

    let (alloc, txt) = match *op_ty {
        Operand::Indirect(place) => {
            let size: u64 = op_ty.layout.size.bytes();
            if place.meta == MemPlaceMeta::None {
                let ptr = place.to_ref().to_scalar().unwrap();
                if ptr.is_ptr() {
                    let (alloc, txt, _len) = print_ptr(ecx, ptr.assert_ptr(), Some(size))?;
                    (alloc, txt)
                } else {
                    (None, format!("{:?}", ptr))
                }
            } else {
                (None, format!("{:?}", place)) // FIXME better printing for unsized locals
            }
        }
        Operand::Immediate(Immediate::Scalar(scalar)) => (None, print_scalar_maybe_undef(scalar)),
        Operand::Immediate(Immediate::ScalarPair(val, extra)) => (
            None,
            format!(
                "{}, {}",
                print_scalar_maybe_undef(val),
                print_scalar_maybe_undef(extra)
            ),
        ),
    };
    let txt = if let Ok(pretty) = pretty {
        format!("{} ({})", pretty, txt)
    } else {
        txt
    };
    Ok((alloc, txt))
}

pub fn print_ptr(
    ecx: &InterpCx,
    ptr: Pointer<Tag>,
    size: Option<u64>,
) -> Result<(Option<u64>, String, u64), ()> {
    if let Ok(alloc) = ecx.memory.get_raw(ptr.alloc_id) {
        let s = print_alloc(ecx.tcx.data_layout.pointer_size.bytes(), ptr, alloc, size);
        debug_assert!(ecx.memory.get_fn(ptr.into()).is_err());
        Ok((Some(ptr.alloc_id.0), s, alloc.len() as u64))
    } else if let Ok(_) = ecx.memory.get_fn(ptr.into()) {
        // FIXME: print function name
        Ok((None, "function pointer".to_string(), 16))
    } else {
        Err(())
    }
}

pub fn print_alloc(
    ptr_size: u64,
    ptr: Pointer<Tag>,
    alloc: &Allocation<Tag, AllocExtra>,
    size: Option<u64>,
) -> String {
    use std::fmt::Write;
    let end = size
        .map(|s| s + ptr.offset.bytes())
        .unwrap_or(alloc.len() as u64);
    let mut s = String::new();
    let mut i = ptr.offset.bytes();
    while i < end {
        if let Some((_tag, reloc)) = alloc.relocations().get(&Size::from_bytes(i)) {
            i += ptr_size;
            write!(&mut s,
                "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                alloc = reloc,
                offset = ptr.offset.bytes(),
                nil = "",
                wdt = (ptr_size * 2 - 2) as usize,
            ).unwrap();
        } else {
            if alloc
                .init_mask()
                .is_range_initialized(Size::from_bytes(i), Size::from_bytes(i + 1))
                .is_ok()
            {
                let byte = alloc
                    .inspect_with_undef_and_ptr_outside_interpreter(i as usize..i as usize + 1)[0];
                write!(&mut s, "{:02x}", byte).unwrap();
            } else {
                let ub_chars = [
                    '∅', '∆', '∇', '∓', '∞', '⊙', '⊠', '⊘', '⊗', '⊛', '⊝', '⊡', '⊠',
                ];
                let c1 = (ptr.alloc_id.0 * 769 + i as u64 * 5689) as usize % ub_chars.len();
                let c2 = (ptr.alloc_id.0 * 997 + i as u64 * 7193) as usize % ub_chars.len();
                write!(&mut s, "<mark>{}{}</mark>", ub_chars[c1], ub_chars[c2]).unwrap();
            }
            i += 1;
        }
    }
    s
}
