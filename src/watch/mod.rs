use std::collections::HashMap;
use std::fmt::Write;

use rustc::ty::Instance;
use rustc::mir::interpret::{Pointer, Allocation};
use rustc::ty::layout::Size;

use ::*;

mod stack_trace;

#[derive(Debug)]
pub struct Traces<'tcx> {
    alloc_traces: HashMap<AllocId, AllocTrace>,
    stack_traces_cpu: Vec<(Vec<(Instance<'tcx>,)>, u128)>,
    stack_traces_mem: Vec<(Vec<(Instance<'tcx>,)>, u128)>,
}

impl<'tcx> Traces<'tcx> {
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

    /// Clear the traces. This should be called before restarting the evaluation.
    pub fn clear(&mut self) {
        // We have to replace all values of alloc_traces by empty AllocTraces,
        // because stepping back will change the alloc id's
        self.alloc_traces.clear();

        // We can just empty the stack traces, because they will be rebuild during stepping
        self.stack_traces_cpu.clear();
        self.stack_traces_mem.clear();
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
    {
        let ecx = &mut pcx.ecx;
        let traces = &mut pcx.traces;
        let step_count = *pcx.step_count;

        // Remove all interned statics
        traces.alloc_traces.retain(|alloc_id, alloc_trace| {
            /*if ecx.tcx.tcx.interpret_interner.get_alloc(*alloc_id).is_none() {
                true
            } else {
                assert!(alloc_trace.trace_points.len() == 0, "at {} {}: {:#?}", step_count, alloc_id, alloc_trace);
                false
            }*/
            true
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
    }

    stack_trace::step_callback(pcx);
}

pub fn routes() -> Vec<::rocket::Route> {
    routes![watch::show, watch::continue_and_show, watch::add]
}

#[get("/show")]
pub fn show(sender: State<PrirodaSender>) -> RResult<Html<String>> {
    sender.do_work(move |pcx| {
        let mut buf = String::new();

        stack_trace::show(pcx, &mut buf).unwrap();

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
                            Pointer::new(*alloc_id, Size::from_bytes(0)).into(),
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

        Html(buf)
    })
}

#[get("/continue_and_show")]
pub fn continue_and_show(sender: State<PrirodaSender>) -> RResult<Html<String>> {
    sender.do_work(move |pcx| {
        ::step::step(pcx, |_ecx| ::step::ShouldContinue::Continue);
    })?;
    show(sender)
}

action_route!(add: "/add/<id>", |pcx, id: u64| {
    pcx.traces.alloc_traces.insert(AllocId(id), AllocTrace::new());
    step_callback(pcx);
    "".to_string()
});
