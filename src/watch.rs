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
    let ecx = &pcx.ecx;
    let step_count = *pcx.step_count;

    // Remove all interned statics
    pcx.traces.alloc_traces.retain(|alloc_id, alloc_trace| {
        if ecx.tcx.tcx.interpret_interner.get_alloc(*alloc_id).is_none() {
            true
        } else {
            assert!(alloc_trace.trace_points.len() == 0, "at {} {}: {:#?}", step_count, alloc_id, alloc_trace);
            false
        }
    });

    // Collect alloc traces
    for (alloc_id, alloc_trace) in pcx.traces.alloc_traces.iter_mut() {
        if let Ok(alloc) = pcx.ecx.memory.get(*alloc_id) {
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
    let stack_trace = ecx.stack().iter().map(|frame| {
        (frame.instance.def_id(),)
    }).collect::<Vec<_>>();
    insert_stack_trace(&mut pcx.traces.stack_traces_cpu, stack_trace.clone());

    let top_fn_def_id = ecx.stack().last().unwrap().instance.def_id();
    let top_fn_attrs = ecx.tcx.get_attrs(top_fn_def_id);
    let top_fn_item_path = ecx.tcx.absolute_item_path_str(top_fn_def_id);
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
