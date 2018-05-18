use std::fmt::Write;
use std::io;
use std::io::Write as IoWrite;
use std::process::Command;

use rustc::ty::{Instance, InstanceDef};

use ::*;

pub(super) fn step_callback(pcx: &mut PrirodaContext) {
    let ecx = &mut pcx.ecx;
    let traces = &mut pcx.traces;

    let mut stack_trace = ecx.stack().iter().map(|frame| {
        (frame.instance,)
    }).collect::<Vec<_>>();
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

    let (stmt, blck) = {
        let frame = ecx.stack().last().unwrap();
        let blck = &frame.mir.basic_blocks()[frame.block];
        (frame.stmt, blck)
    };
    if stmt == blck.statements.len() {
        use rustc::mir::TerminatorKind::*;
        match blck.terminator().kind {
            Call { ref func, ref args, .. } => {
                let instance = instance_for_call_operand(ecx, func);
                let item_path = ecx.tcx.absolute_item_path_str(instance.def_id());
                println!("{}", item_path);

                stack_trace.push((instance,));
                insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

                let _: ::miri::EvalResult = do catch {
                    let args = args
                        .into_iter()
                        .map(|op| ecx.eval_operand(op))
                        .collect::<Result<Vec<_>, _>>()?;
                    match &item_path[..] {
                        "alloc::alloc::::__rust_alloc" | "alloc::alloc::::__rust_alloc_zeroed" => {
                            let size = ecx.value_to_primval(args[0])?.to_u64()?;
                            insert_stack_trace(&mut traces.stack_traces_mem, (stack_trace, true), size as u128);
                        }
                        "alloc::alloc::::__rust_realloc" => {
                            let old_size = ecx.value_to_primval(args[1])?.to_u64()?;
                            let new_size = ecx.value_to_primval(args[3])?.to_u64()?;
                            if new_size > old_size {
                                insert_stack_trace(&mut traces.stack_traces_mem, (stack_trace, true), (new_size - old_size) as u128);
                            }
                        }
                        "alloc::alloc::::__rust_dealloc" => {
                            insert_stack_trace(&mut traces.stack_traces_mem, (stack_trace, false), 1);
                        }
                        _ => {}
                    }
                };
            }
            _ => {}
        }
    }
}

fn instance_for_call_operand<'a, 'tcx: 'a>(ecx: &mut EvalContext<'a, 'tcx>, func: &'tcx ::rustc::mir::Operand) -> Instance<'tcx> {
    let res: ::miri::EvalResult<Instance> = do catch {
        let func = ecx.eval_operand(func)?;

        match func.ty.sty {
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
                (err!(Unimplemented(msg)) as ::miri::EvalResult<_>)?;
                unreachable!()
            }
        }
    };
    res.unwrap()
}

fn insert_stack_trace<T: Eq>(traces: &mut Vec<(T, u128)>, trace: T, count: u128) {
    if traces.last().map(|t|&t.0) == Some(&trace) {
        traces.last_mut().unwrap().1 += count;
    } else {
        traces.push((trace, count));
    }
}

pub(super) fn show(pcx: &mut PrirodaContext, buf: &mut impl Write) -> io::Result<()> {
    create_flame_graph(&pcx.ecx, &mut *buf, &pcx.traces.stack_traces_cpu, |t| t, "Cpu usage", "instructions", "java", "flame_graph_cpu")?;
    create_flame_graph(&pcx.ecx, &mut *buf, &pcx.traces.stack_traces_mem, |t| &t.0, "Memory usage", "bytes", "mem", "flame_graph_mem")?;

    Ok(())
}

fn create_flame_graph<'a, 'tcx: 'a, T>(ecx: &EvalContext<'a, 'tcx>, mut buf: impl Write, traces: &Vec<(T, u128)>, get_trace: impl Fn(&T) -> &[(Instance<'tcx>,)], name: &str, count_name: &str, color_scheme: &str, file_name: &str) -> io::Result<()> {
    let flame_data_file = format!("./resources/{}.txt", file_name);
    let mut flame_file = ::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open(&flame_data_file)?;
    for (stack_trace, count) in traces {
        writeln!(flame_file, "{} {}", get_trace(stack_trace).iter().map(|(instance,)| {
            let mut name = ecx.tcx.absolute_item_path_str(instance.def_id());
            match instance.def {
                InstanceDef::Intrinsic(..) => name.push_str("_[k]"),
                InstanceDef::DropGlue(..) => name.push_str("_[j]"),
                _ => {}
            }
            name
        }).collect::<Vec<_>>().join(";"), count)?;
    }
    ::std::mem::drop(flame_file);

    let res = Command::new("../FlameGraph/flamegraph.pl")
        .arg(flame_data_file)
        .arg("--title").arg(name)
        .arg("--countname").arg(count_name)
        .arg("--colors").arg(color_scheme)
        .stdout(::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open(&format!("./resources/{}.svg", file_name)).unwrap())
        .status();
    if let Err(err) = res {
        writeln!(buf, "<h1><pre>Error: {:?}</pre></h1>", err).unwrap();
    } else {
        writeln!(buf, "{}", ::std::fs::read_to_string(format!("./resources/{}.svg", file_name)).unwrap()).unwrap();
    }

    Ok(())
}
