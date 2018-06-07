use rustc::mir::{self, interpret::EvalErrorKind};
use rustc::ty::{
    layout::{Abi, LayoutOf, Size},
    Ty, TyS, TypeAndMut, TypeVariants,
};
use rustc_data_structures::indexed_vec::Idx;

use miri::{Allocation, EvalResult, Frame, Place, PlaceExtra, Pointer, Scalar, ValTy, Value};

use horrorshow::prelude::*;
use horrorshow::Template;

use EvalContext;

pub fn render_locals<'a, 'tcx: 'a>(
    ecx: &EvalContext<'a, 'tcx>,
    frame: &Frame<'tcx, 'tcx>,
) -> String {
    let &Frame {
        instance,
        ref locals,
        ref mir,
        ref return_place,
        ..
    } = frame;

    //               name    ty      alloc        val     style
    let locals: Vec<(String, String, Option<u64>, String, &str)> = locals
        .iter()
        .enumerate()
        .map(|(id, &val)| {
            let mut val = val;
            let local_decl = &mir.local_decls[mir::Local::new(id)];
            let name = local_decl
                .name
                .map(|n| n.as_str().to_string())
                .unwrap_or_else(String::new);
            let ty = ecx.monomorphize(local_decl.ty, instance.substs);
            if id == 0 {
                val = match *return_place {
                    Place::Ptr { ptr, align, extra } => {
                        if extra != PlaceExtra::None {
                            return (
                                name,
                                ty.to_string(),
                                None,
                                "&lt;unsupported&gt;".to_owned(),
                                "color: red;",
                            );
                        }
                        Some(Value::ByRef(ptr, align))
                    }
                    Place::Local { frame, local } => ecx.stack()[frame].get_local(local).ok(),
                };
            }
            let (alloc, val, style) = match val.map(|value| print_value(ecx, ty, value)) {
                Some(Ok((alloc, text))) => (alloc, text, ""),
                Some(Err(())) => (None, "&lt;error&gt;".to_owned(), "color: red;"),
                None => (None, "&lt;uninit&gt;".to_owned(), "font-size: 0;"),
            };
            (name, ty.to_string(), alloc, val, style)
        }).collect();

    let (arg_count, var_count, tmp_count) = (
        mir.args_iter().count(),
        mir.vars_iter().count(),
        mir.temps_iter().count(),
    );

    (html! {
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
                    } else if i == arg_count + 1 {
                        th(rowspan=var_count) { span(class="vertical") { : "Variables" } }
                    } else if i == var_count + arg_count + 1 {
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

fn print_primval(val: Scalar) -> String {
    match val {
        Scalar::Bits { defined: 0, .. } => "&lt;undef &gt;".to_string(),
        Scalar::Ptr(ptr) => format!(
            "<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>",
            alloc = ptr.alloc_id.0,
            offset = ptr.offset.bytes()
        ),
        Scalar::Bits { bits, defined } => {
            format!("0x{:0width$X}", bits, width = (defined as usize) / 8)
        }
    }
}

fn pp_value<'a, 'tcx: 'a>(
    ecx: &EvalContext<'a, 'tcx>,
    ty: Ty<'tcx>,
    val: Value,
) -> EvalResult<'tcx, String> {
    let layout = ecx.layout_of(ty).unwrap();
    match ty.sty {
        TypeVariants::TyRawPtr(TypeAndMut {
            ty: &TyS {
                sty: TypeVariants::TyStr,
                ..
            },
            ..
        })
        | TypeVariants::TyRef(
            _,
            &TyS {
                sty: TypeVariants::TyStr,
                ..
            },
            _,
        ) => {
            if let Value::ScalarPair(Scalar::Ptr(ptr), Scalar::Bits { bits: len, .. }) = val {
                if let Ok(allocation) = ecx.memory.get(ptr.alloc_id) {
                    let offset = ptr.offset.bytes();
                    if (offset as u128) < allocation.bytes.len() as u128 {
                        let alloc_bytes =
                            &allocation.bytes[offset as usize
                                                  ..(offset as usize)
                                                      .checked_add(len as usize)
                                                      .ok_or(EvalErrorKind::AssumptionNotHeld)?];
                        let s = String::from_utf8_lossy(alloc_bytes);
                        (Ok(format!("\"{}\"", s)) as EvalResult<String>)?;
                    }
                }
            }
        }
        _ => {}
    }
    if layout.size.bytes() == 0 {
        Err(EvalErrorKind::AssumptionNotHeld)?;
    }
    if let Abi::Scalar(_) = layout.abi {
    } else {
        Err(EvalErrorKind::AssumptionNotHeld)?;
    }
    let bits = ecx
        .value_to_scalar(ValTy { value: val, ty })?
        .to_bits(layout.size)?;
    match ty.sty {
        TypeVariants::TyBool => {
            if bits == 0 {
                Ok("false".to_string())
            } else if bits == 1 {
                Ok("true".to_string())
            } else {
                Err(EvalErrorKind::AssumptionNotHeld.into())
            }
        }
        TypeVariants::TyChar if bits < ::std::char::MAX as u128 => {
            let chr = ::std::char::from_u32(bits as u32).unwrap();
            if chr.is_ascii() {
                Ok(format!("'{}'", chr))
            } else {
                Err(EvalErrorKind::AssumptionNotHeld.into())
            }
        }
        TypeVariants::TyUint(_) => Ok(format!("{0}", bits)),
        TypeVariants::TyInt(_) => Ok(format!("{0}", bits as i128)),
        TypeVariants::TyFloat(float_ty) => {
            use syntax::ast::FloatTy::*;
            match float_ty {
                F32 if bits < ::std::u32::MAX as u128 => {
                    Ok(format!("{}", <f32>::from_bits(bits as u32)))
                }
                F64 if bits < ::std::u64::MAX as u128 => {
                    Ok(format!("{}", <f64>::from_bits(bits as u64)))
                }
                _ => Err(EvalErrorKind::AssumptionNotHeld.into()),
            }
        }
        _ => Err(EvalErrorKind::AssumptionNotHeld.into()),
    }
}

pub fn print_value<'a, 'tcx: 'a>(
    ecx: &EvalContext<'a, 'tcx>,
    ty: Ty<'tcx>,
    val: Value,
) -> Result<(Option<u64>, String), ()> {
    let pretty = pp_value(ecx, ty, val);

    let (alloc, txt) = match val {
        Value::ByRef(ptr, _align) => {
            let (alloc, txt, _len) = print_ptr(ecx, ptr)?;
            (alloc, txt)
        }
        Value::Scalar(primval) => (None, print_primval(primval)),
        Value::ScalarPair(val, extra) => (
            None,
            format!("{}, {}", print_primval(val), print_primval(extra)),
        ),
    };
    let txt = if let Ok(pretty) = pretty {
        format!("{} ({})", pretty, txt)
    } else {
        txt
    };
    Ok((alloc, txt))
}

pub fn print_ptr(ecx: &EvalContext, ptr: Scalar) -> Result<(Option<u64>, String, u64), ()> {
    let ptr = ptr.to_ptr().map_err(|_| ())?;
    match (ecx.memory().get(ptr.alloc_id), ecx.memory().get_fn(ptr)) {
        (Ok(alloc), Err(_)) => {
            let s = print_alloc(ecx.memory().pointer_size().bytes(), ptr, alloc);
            Ok((Some(ptr.alloc_id.0), s, alloc.bytes.len() as u64))
        }
        (Err(_), Ok(_)) => {
            // FIXME: print function name
            Ok((None, "function pointer".to_string(), 16))
        }
        (Err(_), Err(_)) => Err(()),
        (Ok(_), Ok(_)) => unreachable!(),
    }
}

pub fn print_alloc(ptr_size: u64, ptr: Pointer, alloc: &Allocation) -> String {
    use std::fmt::Write;
    let mut s = String::new();
    let mut i = 0;
    while i < alloc.bytes.len() as u64 {
        if let Some(&reloc) = alloc.relocations.get(&Size::from_bytes(i)) {
            i += ptr_size;
            write!(&mut s,
                "<a style=\"text-decoration: none\" href=\"/ptr/{alloc}/{offset}\">┠{nil:─<wdt$}┨</a>",
                alloc = reloc.0,
                offset = ptr.offset.bytes(),
                nil = "",
                wdt = (ptr_size * 2 - 2) as usize,
            ).unwrap();
        } else {
            if alloc
                .undef_mask
                .is_range_defined(Size::from_bytes(i), Size::from_bytes(i + 1))
            {
                write!(&mut s, "{:02x}", alloc.bytes[i as usize] as usize).unwrap();
            } else {
                let ub_chars = [
                    '∅', '∆', '∇', '∓', '∞', '⊙', '⊠', '⊘', '⊗', '⊛', '⊝',
                    '⊡', '⊠',
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
