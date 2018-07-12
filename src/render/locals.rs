use rustc::mir;
use rustc::ty::{layout::Size, Ty, TyS, TypeAndMut, TypeVariants};
use rustc_data_structures::indexed_vec::Idx;

use miri::{Allocation, Frame, Place, PlaceExtra, Pointer, Scalar, Value};

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
                Some((alloc, text)) => (alloc, text, ""),
                None => (None, "&lt;uninit&gt;".to_owned(), "font-size: 0;"),
            };
            (name, ty.to_string(), alloc, val, style)
        })
        .collect();

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

fn print_primval(ty: Option<Ty>, val: Scalar) -> String {
    match val {
        Scalar::Bits { defined: 0, .. } => "&lt;undef &gt;".to_string(),
        Scalar::Ptr(ptr) => format!(
            "<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>",
            alloc = ptr.alloc_id.0,
            offset = ptr.offset.bytes()
        ),
        Scalar::Bits { bits, .. } => match ty {
            Some(&TyS {
                sty: TypeVariants::TyBool,
                ..
            }) => {
                if bits == 0 {
                    "false (0)".to_string()
                } else if bits == 1 {
                    "true (1)".to_string()
                } else {
                    bits.to_string()
                }
            }
            Some(&TyS {
                sty: TypeVariants::TyChar,
                ..
            })
                if bits < ::std::u32::MAX as u128 =>
            {
                let chr = ::std::char::from_u32(bits as u32).unwrap();
                if chr.is_ascii() {
                    format!("'{}' (0x{:08X})", chr, bits)
                } else {
                    bits.to_string()
                }
            }
            Some(&TyS {
                sty: TypeVariants::TyUint(_),
                ..
            }) => format!("{0} (0x{0:08X})", bits),
            Some(&TyS {
                sty: TypeVariants::TyInt(_),
                ..
            }) => format!("{0} (0x{0:08X})", bits as i128),
            Some(&TyS {
                sty: TypeVariants::TyFloat(float_ty),
                ..
            }) => {
                use syntax::ast::FloatTy::*;
                match float_ty {
                    F32 if bits < ::std::u32::MAX as u128 => {
                        format!("{} (0x{:08X})", <f32>::from_bits(bits as u32), bits as u32)
                    }
                    F64 if bits < ::std::u64::MAX as u128 => {
                        format!("{} (0x{:08X})", <f64>::from_bits(bits as u64), bits as u64)
                    }
                    _ => bits.to_string(),
                }
            }
            _ => bits.to_string(),
        },
    }
}

fn print_value(ecx: &EvalContext, ty: Ty, val: Value) -> (Option<u64>, String) {
    let txt = match val {
        Value::ByRef(ptr, _align) => {
            if let Ok((alloc, txt, _len)) = print_ptr(ecx, ptr) {
                return (alloc, txt);
            } else {
                return (None, format!("{:?} does not exist", val));
            }
        }
        Value::Scalar(primval) => print_primval(Some(ty), primval),
        Value::ScalarPair(val, extra) => {
            match ty.sty {
                TypeVariants::TyRawPtr(TypeAndMut {
                    ty:
                        &TyS {
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
                    if let (Scalar::Ptr(ptr), Scalar::Bits { bits: len, .. }) = (val, extra) {
                        if let Ok(allocation) = ecx.memory.get(ptr.alloc_id) {
                            if (ptr.offset.bytes() as u128) < allocation.bytes.len() as u128 {
                                let alloc_bytes = &allocation.bytes[ptr.offset.bytes() as usize..];
                                let s = String::from_utf8_lossy(alloc_bytes);
                                return (
                                    None,
                                    format!("\"{}\" ({}, {})", s, print_primval(None, val), len),
                                );
                            }
                        }
                    }
                }
                _ => {}
            }
            format!(
                "{}, {}",
                print_primval(None, val),
                print_primval(None, extra)
            )
        }
    };
    (None, txt)
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
