use std::collections::HashMap;
use std::fmt::Write;
use std::io::Write as IoWrite;

use rustc::hir::def_id::DefId;
use rustc::mir::interpret::{MemoryPointer, Allocation};
use rustc::ty::layout::Size;

use ::*;

#[derive(Debug)]
pub struct Traces {
    alloc_traces: HashMap<AllocId, AllocTrace>,
    stack_traces_cpu: Vec<(Vec<(DefId,)>, u128)>,
    stack_traces_mem: Vec<((Vec<(DefId,)>, bool), u128)>,
}

impl Traces {
    pub fn new() -> Self {
        let alloc_traces = HashMap::new();
        //for i in 0..700 {
        //    alloc_traces.insert(AllocId(i), AllocTrace::new());
        //}
        Traces {
            alloc_traces,
            stack_traces_cpu: Vec::new(),
            stack_traces_mem: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct AllocTrace {
    trace_points: Vec<(u128, AllocTracePoint)>,
}

impl AllocTrace {
    fn new() -> Self {
        AllocTrace {
            trace_points: Vec::new(),
        }
    }
}

#[derive(Debug)]
enum AllocTracePoint {
    Changed(Allocation),
    Deallocated,
}

pub fn step_callback(pcx: &mut PrirodaContext) {
    let ecx = &mut pcx.ecx;
    let traces = &mut pcx.traces;
    let step_count = *pcx.step_count;

    // Remove all interned statics
    traces.alloc_traces.retain(|alloc_id, alloc_trace| {
        if ecx.tcx.tcx.interpret_interner.get_alloc(*alloc_id).is_none() {
            true
        } else {
            assert!(alloc_trace.trace_points.len() == 0, "at {} {}: {:#?}", step_count, alloc_id, alloc_trace);
            false
        }
    });

    // Collect alloc traces
    for (alloc_id, alloc_trace) in traces.alloc_traces.iter_mut() {
        if let Ok(alloc) = ecx.memory.get(*alloc_id) {
            if let Some(&(prev_step_count, AllocTracePoint::Changed(ref prev_alloc))) = alloc_trace.trace_points.last() {
                if alloc == prev_alloc || *pcx.step_count == prev_step_count {
                    continue;
                }
            }

            alloc_trace.trace_points.push((*pcx.step_count, AllocTracePoint::Changed(Allocation {
                bytes: alloc.bytes.clone(),
                relocations: alloc.relocations.clone(),
                undef_mask: alloc.undef_mask.clone(),
                align: alloc.align.clone(),
                runtime_mutability: alloc.runtime_mutability.clone(),
            })));
        } else {
            if let Some(&(_, AllocTracePoint::Deallocated)) = alloc_trace.trace_points.last() {
            } else if alloc_trace.trace_points.is_empty() {
            } else {
                alloc_trace.trace_points.push((*pcx.step_count, AllocTracePoint::Deallocated));
            }
        }
    }

    // Collect stack traces
    let mut stack_trace = ecx.stack().iter().map(|frame| {
        (frame.instance.def_id(),)
    }).collect::<Vec<_>>();
    insert_stack_trace(&mut traces.stack_traces_cpu, stack_trace.clone());

    let (stmt, blck) = {
        let frame = ecx.stack().last().unwrap();
        let blck = &frame.mir.basic_blocks()[frame.block];;
        (frame.stmt, blck)
    };
    if stmt == blck.statements.len() {
        use rustc::mir::TerminatorKind::*;
        match blck.terminator().kind {
            Call { ref func, .. } => {
                let res: ::miri::EvalResult = (||{
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

#[get("/show")]
pub fn show(sender: State<PrirodaSender>) -> RResult<Html<String>> {
    sender.do_work(move |pcx| {
        let mut buf = String::new();
        let mut alloc_traces = pcx.traces.alloc_traces.iter().collect::<Vec<_>>();
        alloc_traces.sort_by_key(|(id, _)| id.0);
        for (alloc_id, alloc_trace) in alloc_traces {
            if alloc_trace.trace_points.is_empty() {
                writeln!(buf, "<h2>Alloc {} has never existed</h2>", alloc_id.0).unwrap();
                continue;
            }

            writeln!(buf, "<h2>Alloc {}</h2>\n<table border='1'>", alloc_id.0).unwrap();
            for (step_count, trace_point) in &alloc_trace.trace_points {
                let content = match trace_point {
                    AllocTracePoint::Changed(alloc) => {
                        ::render::locals::print_alloc(
                            pcx.ecx.memory().pointer_size().bytes(),
                            MemoryPointer::new(*alloc_id, Size::from_bytes(0)).into(),
                            alloc
                        )
                    }
                    AllocTracePoint::Deallocated => "Dealloc".to_string(),
                };
                writeln!(
                    buf,
                    "<tr>\n<td>{}</td>\n<td>{}</td>\n</tr>",
                    step_count,
                    content
                ).unwrap();
            }
            writeln!(buf, "</table>\n").unwrap();
        }

        let mut flame_file_cpu = ::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("./flame_graph_cpu.txt").unwrap();
        for (stack_trace, count) in &pcx.traces.stack_traces_cpu {
            writeln!(flame_file_cpu, "{} {}", stack_trace.iter().map(|(def_id,)| {
                pcx.ecx.tcx.absolute_item_path_str(*def_id)
            }).collect::<Vec<_>>().join(";"), count).unwrap();
        }

        let mut flame_file_mem = ::std::fs::OpenOptions::new().write(true).truncate(true).create(true).open("./flame_graph_mem.txt").unwrap();
        for ((stack_trace, _), count) in &pcx.traces.stack_traces_mem {
            writeln!(flame_file_mem, "{} {}", stack_trace.iter().map(|(def_id,)| {
                pcx.ecx.tcx.absolute_item_path_str(*def_id)
            }).collect::<Vec<_>>().join(";"), count).unwrap();
        }
        Html(buf)
        //format!("{:#?}", pcx.alloc_traces)
    })
}

action_route!(add: "/add/<id>", |pcx, id: u64| {
    pcx.traces.alloc_traces.insert(AllocId(id), AllocTrace::new());
    step_callback(pcx);
    "".to_string()
});
