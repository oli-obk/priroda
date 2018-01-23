use rustc_data_structures::indexed_vec::Idx;
use rustc::ty::{TyCtxt, Ty, TyS, TypeVariants, TypeAndMut};
use rustc::mir;

use miri::{
    Frame,
    MemoryPointer,
    Value,
    PrimVal,
    Pointer,
    AllocId,
};

use horrorshow::prelude::*;
use horrorshow::Template;

use EvalContext;

pub fn render_locals<'a, 'tcx: 'a>(_tcx: TyCtxt<'a, 'tcx, 'tcx>, ecx: &EvalContext<'a, 'tcx>, frame: Option<&Frame<'tcx>>) -> String {
    //               name    ty      alloc        val     style
    let locals: Vec<(String, String, Option<u64>, String, &str)> = frame.map_or(Vec::new(), |&Frame { instance, ref locals, ref mir, ref return_place, .. }| {
        let ret_val = ecx.read_place(*return_place).ok();
        ::std::iter::once(&ret_val).chain(locals.iter()).enumerate()
            .map(|(id, &val)| {
                let local_decl = &mir.local_decls[mir::Local::new(id)];
                let name = local_decl.name.map(|n|n.as_str().to_string()).unwrap_or_else(||String::new());
                let ty = ecx.monomorphize(local_decl.ty, instance.substs);
                match val.map(|value| print_value(ecx, ty, value)) {
                    Some(Ok((alloc, text))) => (name, ty.to_string(), alloc, text, ""),
                    Some(Err(())) => (name, ty.to_string(), None, format!("{:?} does not exist", val), ""),
                    None => (name, ty.to_string(), None, "&lt;uninit&gt;".to_owned(), "font-size: 0;"),
                }
            }).collect()
    });

    let (arg_count, var_count, tmp_count) = frame.map_or((0, 0, 0), |&Frame { ref mir, .. }| (
        mir.args_iter().count(),
        mir.vars_iter().count(),
        mir.temps_iter().count(),
    ));

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
    }).into_string().unwrap()
}

pub fn print_primval(ty: Option<Ty>, val: PrimVal) -> String {
    match val {
        PrimVal::Undef => "&lt;undef &gt;".to_string(),
        PrimVal::Ptr(ptr) => format!("<a href=\"/ptr/{alloc}/{offset}\">Pointer({alloc})[{offset}]</a>", alloc = ptr.alloc_id.0, offset = ptr.offset),
        PrimVal::Bytes(bytes) => {
            match ty {
                Some(&TyS { sty: TypeVariants::TyBool, ..}) => {
                    if bytes == 0 {
                        return "false (0)".to_string()
                    } else if bytes == 1 {
                        return "true (1)".to_string()
                    }
                }
                Some(&TyS { sty: TypeVariants::TyChar, ..}) => {
                    if bytes < ::std::u32::MAX as u128 {
                        let chr = ::std::char::from_u32(bytes as u32).unwrap();
                        if chr.is_ascii() {
                            return format!("'{}' (0x{:08X})", chr, bytes);
                        }
                    }
                }
                Some(&TyS { sty: TypeVariants::TyUint(_), ..}) => {
                    return format!("{0} (0x{0:08X})", bytes);
                }
                Some(&TyS { sty: TypeVariants::TyInt(_), ..}) => {
                    return format!("{0} (0x{0:08X})", bytes as i128);
                }
                Some(&TyS { sty: TypeVariants::TyFloat(float_ty), ..}) => {
                    use syntax::ast::FloatTy::*;
                    match float_ty {
                        F32 => {
                            if bytes < ::std::u32::MAX as u128 {
                                return format!("{} (0x{:08X})", <f32>::from_bits(bytes as u32), bytes as u32);
                            }
                        }
                        F64 => {
                            if bytes < ::std::u64::MAX as u128 {
                                return format!("{} (0x{:08X})", <f64>::from_bits(bytes as u64), bytes as u64);
                            }
                        }
                    }
                }
                _ => {},
            }
            bytes.to_string()
        },
    }
}

pub fn print_value(ecx: &EvalContext, ty: Ty, val: Value) -> Result<(Option<u64>, String), ()> {
    let txt = match val {
        Value::ByRef(ptr, _align) => {
            let (alloc, txt, _len) = print_ptr(ecx, ptr)?;
            return Ok((alloc, txt));
        },
        Value::ByVal(primval) => print_primval(Some(ty), primval),
        Value::ByValPair(val, extra) => {
            match ty.sty {
                TypeVariants::TyRawPtr(TypeAndMut { ty: &TyS { sty: TypeVariants::TyStr, .. }, .. }) |
                TypeVariants::TyRef(_, TypeAndMut { ty: &TyS { sty: TypeVariants::TyStr, .. }, .. }) => {
                    if let (PrimVal::Ptr(ptr), PrimVal::Bytes(extra)) = (val, extra) {
                        if let Ok(allocation) = ecx.memory.get(ptr.alloc_id) {
                            if (ptr.offset as u128) < allocation.bytes.len() as u128 {
                                let bytes = &allocation.bytes[ptr.offset as usize..];
                                let s = String::from_utf8_lossy(bytes);
                                return Ok((None, format!("\"{}\" ({}, {})", s, print_primval(None, val), extra)));
                            }
                        }
                    }
                }
                _ => {}
            }
            format!("{}, {}", print_primval(None, val), print_primval(None, extra))
        },
    };
    Ok((None, txt))
}

pub fn print_ptr(ecx: &EvalContext, ptr: Pointer) -> Result<(Option<u64>, String, u64), ()> {
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
