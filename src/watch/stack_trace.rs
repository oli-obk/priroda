use std::string::ToString;
use std::fmt::Write;
use std::io;
use std::io::Write as IoWrite;
use std::process::Command;

use rustc::hir::def_id::DefId;

use ::*;

pub(super) fn step_callback(pcx: &mut PrirodaContext) {
    let ecx = &mut pcx.ecx;
    let traces = &mut pcx.traces;

    let mut stack_trace = ecx.stack().iter().map(|frame| {
        (frame.instance.def_id(),)
    }).collect::<Vec<_>>();
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone());

    let (stmt, blck) = {
        let frame = ecx.stack().last().unwrap();
        let blck = &frame.mir.basic_blocks()[frame.block];
        (frame.stmt, blck)
    };
    if stmt == blck.statements.len() {
        use rustc::mir::TerminatorKind::*;
        match blck.terminator().kind {
            Call { ref func, .. } => {
                // Ignore errors
                let _: ::miri::EvalResult = (||{
                    let func = ecx.eval_operand(func)?;
                    let fn_def = match func.ty.sty {
                        ty::TyFnPtr(_) => {
                            let fn_ptr = ecx.value_to_primval(func)?.to_ptr()?;
                            let instance = ecx.memory.get_fn(fn_ptr)?;
                            instance
                        }
                        ty::TyFnDef(def_id, substs) => {
                            let substs = ecx.tcx.subst_and_normalize_erasing_regions(
                                ecx.substs(),
                                ecx.param_env,
                                &substs,
                            );
                            ty::Instance::resolve(
                                *ecx.tcx,
                                ecx.param_env,
                                def_id,
                                substs,
                            ).unwrap()
                        },
                        _ => {
                            let msg = format!("can't handle callee of type {:?}", func.ty);
                            return err!(Unimplemented(msg));
                        }
                    };

                    let def_id = fn_def.def_id();
                    let item_path = ecx.tcx.absolute_item_path_str(def_id);
                    println!("{}", item_path);

                    stack_trace.push((def_id,));
                    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone());
                    match &item_path[..] {
                        "alloc::alloc::::__rust_alloc" | "alloc::alloc::::__rust_alloc_zeroed" | "alloc::alloc::::__rust_realloc" => {
                            insert_stack_trace(&mut traces.stack_traces_mem, (stack_trace, true));
                        }
                        "alloc::alloc::::__rust_dealloc" => {
                            insert_stack_trace(&mut traces.stack_traces_mem, (stack_trace, false));
                        }
                        _ => {}
                    }
                    Ok(())
                })();
            }
            _ => {}
        }
    }
}

fn insert_stack_trace<T: Eq>(traces: &mut Vec<(T, u128)>, trace: T) {
    if traces.last().map(|t|&t.0) == Some(&trace) {
        traces.last_mut().unwrap().1 += 1;
    } else {
        traces.push((trace, 1));
    }
}

pub(super) fn show(pcx: &mut PrirodaContext, buf: &mut impl Write) -> io::Result<()> {
    create_flame_graph(pcx, &mut *buf, &pcx.traces.stack_traces_cpu, |t| t, "flame_graph_cpu")?;
    create_flame_graph(pcx, &mut *buf, &pcx.traces.stack_traces_mem, |t| &t.0, "flame_graph_mem")?;

    Ok(())
}

fn create_flame_graph<T, P: ToString + ?Sized>(pcx: &PrirodaContext, mut buf: impl Write, traces: &Vec<(T, u128)>, get_trace: impl Fn(&T) -> &[(DefId,)], file_name: &P) -> io::Result<()> {
    let mut flame_file = ::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open(file_name.to_string() + ".txt")?;
    for (stack_trace, count) in traces {
        writeln!(flame_file, "{} {}", get_trace(stack_trace).iter().map(|(def_id,)| {
            pcx.ecx.tcx.absolute_item_path_str(*def_id)
        }).collect::<Vec<_>>().join(";"), count)?;
    }
    ::std::mem::drop(flame_file);

    let res = Command::new("../FlameGraph/flamegraph.pl")
        .arg(file_name.to_string() + ".txt")
        .stdout(::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open(file_name.to_string() + ".svg").unwrap())
        .status();
    writeln!(buf, "{:?}", res).unwrap();

    Ok(())
}
