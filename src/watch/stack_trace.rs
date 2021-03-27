use std::fmt::Write;
use std::io::{self, Write as IoWrite};
use std::process::{Command, Stdio};

use rustc_middle::ty::{self, Instance, InstanceDef, ParamEnv};
use rustc_mir::interpret::Machine;

use crate::*;

pub(super) fn step_callback(pcx: &mut PrirodaContext<'_, '_>) {
    let ecx = &pcx.ecx;
    let traces = &mut pcx.traces;

    let stack_trace = Machine::stack(ecx)
        .iter()
        .map(|frame| (frame.instance,))
        .collect::<Vec<_>>();
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

    let location = if let Some(location) = ecx.frame().current_loc().ok() {
        location
    } else {
        return; // Unwinding, but no cleanup for current frame needed
    };
    let mir::Location {
        block,
        statement_index: stmt,
    } = location;
    let blck = &ecx.frame().body.basic_blocks()[block];

    if stmt == blck.statements.len() {
        use rustc_middle::mir::TerminatorKind::*;
        match &blck.terminator().kind {
            Call { func, args, .. } => {
                if let Some(instance) = instance_for_call_operand(ecx, &func) {
                    insert_stack_traces_for_instance(pcx, stack_trace, instance, Some(&args));
                }
            }
            Drop { place, .. } => {
                let location_ty = place.ty(ecx.frame().body, ecx.tcx.tcx).ty;
                let location_ty = ecx.tcx.subst_and_normalize_erasing_regions(
                    ecx.frame().instance.substs,
                    ParamEnv::reveal_all(),
                    location_ty,
                );
                let instance = Instance::resolve_drop_in_place(ecx.tcx.tcx, location_ty);
                insert_stack_traces_for_instance(pcx, stack_trace, instance, None);
            }
            _ => {}
        }
    }
}

fn instance_for_call_operand<'a, 'tcx: 'a>(
    ecx: &InterpCx<'tcx>,
    func: &'tcx rustc_middle::mir::Operand<'_>,
) -> Option<Instance<'tcx>> {
    let res: ::miri::InterpResult<'_, Instance<'_>> = try {
        let func = ecx.eval_operand(func, None)?;

        match func.layout.ty.kind() {
            ty::FnPtr(_) => {
                let fn_ptr = ecx.read_scalar(&func)?.check_init()?;
                if let Ok(instance) = ecx.memory.get_fn(fn_ptr)?.as_instance() {
                    instance
                } else {
                    return None;
                }
            }
            ty::FnDef(def_id, substs) => {
                let substs = ecx.tcx.subst_and_normalize_erasing_regions(
                    ecx.frame().instance.substs,
                    ParamEnv::reveal_all(),
                    *substs,
                );
                ty::Instance::resolve(*ecx.tcx, ParamEnv::reveal_all(), *def_id, substs)
                    .unwrap()
                    .unwrap()
            }
            _ => {
                panic!("can't handle callee of type {:?}", func.layout.ty);
            }
        }
    };
    Some(res.unwrap())
}

fn insert_stack_traces_for_instance<'a, 'tcx: 'a>(
    pcx: &mut PrirodaContext<'a, 'tcx>,
    mut stack_trace: Vec<(Instance<'tcx>,)>,
    instance: Instance<'tcx>,
    args: Option<&[mir::Operand<'tcx>]>,
) {
    let ecx = &pcx.ecx;
    let traces = &mut pcx.traces;

    let item_path = ecx.tcx.def_path_str(instance.def_id());

    stack_trace.push((instance,));
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

    let _: ::miri::InterpResult<'_> = try {
        let args = if let Some(args) = args {
            args.into_iter()
                .map(|op| ecx.eval_operand(op, None))
                .collect::<Result<Vec<_>, _>>()?
        } else {
            return;
        };

        match &item_path[..] {
            "alloc::alloc::::__rust_alloc" | "alloc::alloc::::__rust_alloc_zeroed" => {
                let size = ecx.read_scalar(&args[0])?.to_machine_usize(&ecx.tcx.tcx)?;
                insert_stack_trace(&mut traces.stack_traces_mem, stack_trace, size as u128);
            }
            "alloc::alloc::::__rust_realloc" => {
                let old_size = ecx.read_scalar(&args[1])?.to_machine_usize(&ecx.tcx.tcx)?;
                let new_size = ecx.read_scalar(&args[3])?.to_machine_usize(&ecx.tcx.tcx)?;
                if new_size > old_size {
                    insert_stack_trace(
                        &mut traces.stack_traces_mem,
                        stack_trace,
                        (new_size - old_size) as u128,
                    );
                }
            }
            _ => {}
        }
    };
}

fn insert_stack_trace<T: Eq>(traces: &mut Vec<(T, u128)>, trace: T, count: u128) {
    if let Some(t) = traces.last_mut() {
        if t.0 == trace {
            t.1 += count;
            return;
        }
    }
    traces.push((trace, count));
}

pub(super) fn show(pcx: &PrirodaContext<'_, '_>, buf: &mut impl Write) -> io::Result<()> {
    writeln!(buf, "{}\n", crate::render::refresh_script(pcx)).unwrap();
    create_flame_graph(
        &pcx.ecx,
        &mut *buf,
        &pcx.traces.stack_traces_cpu,
        "Cpu usage",
        "instructions",
        "java",
        "flame_graph_cpu",
    )?;
    create_flame_graph(
        &pcx.ecx,
        &mut *buf,
        &pcx.traces.stack_traces_mem,
        "Memory usage",
        "bytes",
        "mem",
        "flame_graph_mem",
    )?;
    print_stack_traces(&pcx.ecx, &mut *buf, &pcx.traces.stack_traces_cpu).unwrap();
    Ok(())
}

fn create_flame_graph<'a, 'tcx: 'a>(
    ecx: &InterpCx<'tcx>,
    mut buf: impl Write,
    traces: &[(Vec<(Instance<'tcx>,)>, u128)],
    name: &str,
    count_name: &str,
    color_scheme: &str,
    _file_name: &str,
) -> io::Result<()> {
    let mut flame_data = String::new();
    for (stack_trace, count) in traces {
        let mut last_crate = rustc_hir::def_id::LOCAL_CRATE;
        writeln!(
            flame_data,
            "{} {}",
            stack_trace
                .iter()
                .map(|(instance,)| {
                    let mut name = ecx.tcx.def_path_str(instance.def_id());
                    match instance.def {
                        InstanceDef::Intrinsic(..) => name.push_str("_[k]"),
                        InstanceDef::DropGlue(..) => name.push_str("_[k]"),
                        _ => {
                            if instance.def_id().is_local() {
                                name.push_str("_[j]");
                            }
                        }
                    }
                    if last_crate != instance.def_id().krate {
                        name = "-;".to_string() + &name;
                        last_crate = instance.def_id().krate;
                    }
                    name
                })
                .collect::<Vec<_>>()
                .join(";"),
            count
        )
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
    }

    //::std::fs::write(format!("./resources/{}.txt", _file_name), flame_data.as_bytes())?;

    let child = Command::new("../FlameGraph/flamegraph.pl")
        .arg("-")
        .arg("--title")
        .arg(name)
        .arg("--countname")
        .arg(count_name)
        .arg("--colors")
        .arg(color_scheme)
        .arg("--minwidth")
        .arg("0.01")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn();
    match child {
        Ok(mut child) => {
            child
                .stdin
                .as_mut()
                .unwrap()
                .write_all(flame_data.as_bytes())?;
            match child.wait_with_output() {
                Ok(output) => {
                    let flame_graph = String::from_utf8(output.stdout).unwrap();
                    //::std::fs::write(format!("./resources/{}.svg", _file_name), flame_graph.as_bytes())?;
                    writeln!(buf, "{}", flame_graph).unwrap()
                }
                Err(err) => writeln!(buf, "<h1><pre>Wait error: {:?}</pre></h1>", err).unwrap(),
            }
        }
        Err(err) => writeln!(buf, "<h1><pre>Spawn error: {:?}</pre></h1>", err).unwrap(),
    }

    Ok(())
}

fn print_stack_traces<'a, 'tcx: 'a>(
    ecx: &InterpCx<'tcx>,
    mut buf: impl Write,
    traces: &[(Vec<(Instance<'tcx>,)>, u128)],
) -> ::std::fmt::Result {
    let name_for_instance = |i: Instance<'_>| {
        ecx.tcx
            .def_path_str(i.def_id())
            .replace("<", "&lt;")
            .replace(">", "&gt;")
    };
    writeln!(buf, "<h1>Stack trace</h1>\n<ul>\n")?;
    for (stack_trace, count) in traces {
        writeln!(
            buf,
            "<li>{2}{0} ({1})</li>\n",
            name_for_instance(stack_trace.last().unwrap().0),
            count,
            format!(
                "{nil: <indent$}",
                nil = "",
                indent = 4 * (stack_trace.len() - 1)
            )
            .replace(" ", "&nbsp;")
        )?;
    }
    writeln!(buf, "</ul>")?;
    Ok(())
}
