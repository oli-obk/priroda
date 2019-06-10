use std::fmt::Write;
use std::io::{self, Write as IoWrite};
use std::process::{Command, Stdio};

use crate::rustc::ty::{self, Instance, InstanceDef, ParamEnv};

use crate::*;

pub(super) fn step_callback(pcx: &mut PrirodaContext) {
    let ecx = &mut pcx.ecx;
    let traces = &mut pcx.traces;

    let stack_trace = ecx
        .stack()
        .iter()
        .map(|frame| (frame.instance,))
        .collect::<Vec<_>>();
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

    let (stmt, blck) = {
        let frame = ecx.frame();
        let blck = &frame.mir.basic_blocks()[frame.block];
        (frame.stmt, blck)
    };
    if stmt == blck.statements.len() {
        use crate::rustc::mir::TerminatorKind::*;
        match &blck.terminator().kind {
            Call { func, args, .. } => {
                let instance = instance_for_call_operand(ecx, func);
                insert_stack_traces_for_instance(pcx, stack_trace, instance, Some(args));
            }
            Drop { location, .. } => {
                let location_ty = location.ty(ecx.frame().mir, ecx.tcx.tcx).ty;
                let location_ty = ecx.tcx.subst_and_normalize_erasing_regions(
                    ecx.frame().instance.substs,
                    ParamEnv::reveal_all(),
                    &location_ty,
                );
                let instance = Instance::resolve_drop_in_place(ecx.tcx.tcx, location_ty);
                println!("{:?}", instance);
                insert_stack_traces_for_instance(pcx, stack_trace, instance, None);
            }
            _ => {}
        }
    }
}

fn instance_for_call_operand<'a, 'tcx: 'a>(
    ecx: &mut InterpretCx<'a, 'tcx>,
    func: &'tcx crate::rustc::mir::Operand,
) -> Instance<'tcx> {
    let res: ::miri::InterpResult<Instance> = try {
        let func = ecx.eval_operand(func, None)?;

        match func.layout.ty.sty {
            ty::FnPtr(_) => {
                let fn_ptr = ecx.read_scalar(func)?.to_ptr()?;
                ecx.memory().get_fn(fn_ptr)?
            }
            ty::FnDef(def_id, substs) => {
                let substs = ecx.tcx.subst_and_normalize_erasing_regions(
                    ecx.frame().instance.substs,
                    ParamEnv::reveal_all(),
                    &substs,
                );
                ty::Instance::resolve(*ecx.tcx, ParamEnv::reveal_all(), def_id, substs).unwrap()
            }
            _ => {
                let msg = format!("can't handle callee of type {:?}", func.layout.ty);
                (err!(Unimplemented(msg)) as ::miri::InterpResult<_>)?;
                unreachable!()
            }
        }
    };
    res.unwrap()
}

fn insert_stack_traces_for_instance<'a, 'tcx: 'a>(
    pcx: &mut PrirodaContext<'a, 'tcx>,
    mut stack_trace: Vec<(Instance<'tcx>,)>,
    instance: Instance<'tcx>,
    args: Option<&[mir::Operand<'tcx>]>,
) {
    let ecx = &mut pcx.ecx;
    let traces = &mut pcx.traces;

    let item_path = ecx.tcx.def_path_str(instance.def_id());

    stack_trace.push((instance,));
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone(), 1);

    let _: ::miri::InterpResult = try {
        let args = args
            .ok_or(miri::InterpError::AssumptionNotHeld)?
            .into_iter()
            .map(|op| ecx.eval_operand(op, None))
            .collect::<Result<Vec<_>, _>>()?;
        match &item_path[..] {
            "alloc::alloc::::__rust_alloc" | "alloc::alloc::::__rust_alloc_zeroed" => {
                let size = ecx.read_scalar(args[0])?.to_usize(&ecx.tcx.tcx)?;
                insert_stack_trace(&mut traces.stack_traces_mem, stack_trace, size as u128);
            }
            "alloc::alloc::::__rust_realloc" => {
                let old_size = ecx.read_scalar(args[1])?.to_usize(&ecx.tcx.tcx)?;
                let new_size = ecx.read_scalar(args[3])?.to_usize(&ecx.tcx.tcx)?;
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

pub(super) fn show(pcx: &PrirodaContext, buf: &mut impl Write) -> io::Result<()> {
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
    ecx: &InterpretCx<'a, 'tcx>,
    mut buf: impl Write,
    traces: &[(Vec<(Instance<'tcx>,)>, u128)],
    name: &str,
    count_name: &str,
    color_scheme: &str,
    _file_name: &str,
) -> io::Result<()> {
    let mut flame_data = String::new();
    for (stack_trace, count) in traces {
        let mut last_crate = crate::rustc::hir::def_id::LOCAL_CRATE;
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
    ecx: &InterpretCx<'a, 'tcx>,
    mut buf: impl Write,
    traces: &[(Vec<(Instance<'tcx>,)>, u128)],
) -> ::std::fmt::Result {
    let name_for_instance = |i: Instance| {
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
