use std::collections::HashMap;
use std::fmt::Write;

use rustc_middle::mir::interpret::{Allocation, Pointer, PointerArithmetic};
use rustc_middle::ty::Instance;
use rustc_mir::interpret::AllocId;
use rustc_target::abi::Size;

use crate::*;

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
    Changed(Allocation<miri::Tag, miri::AllocExtra>),
    Deallocated,
}

fn eq_alloc(
    a: &Allocation<miri::Tag, miri::AllocExtra>,
    b: &Allocation<miri::Tag, miri::AllocExtra>,
) -> bool {
    let Allocation {
        align: a_align,
        mutability: a_mut,
        extra: _,
        size: a_size,
        ..
    } = a;
    let Allocation {
        align: b_align,
        mutability: b_mut,
        extra: _,
        size: b_size,
        ..
    } = b;
    a_align == b_align
        && a_mut == b_mut
        && a_size == b_size
        && a.inspect_with_uninit_and_ptr_outside_interpreter(0..a.len())
            == b.inspect_with_uninit_and_ptr_outside_interpreter(0..b.len())
        && a.relocations() == b.relocations()
        && a.init_mask() == b.init_mask()
}

pub fn step_callback(pcx: &mut PrirodaContext<'_, '_>) {
    {
        let ecx = &pcx.ecx;
        let traces = &mut pcx.traces;

        // Collect alloc traces
        for (alloc_id, alloc_trace) in &mut traces.alloc_traces {
            if let Ok(alloc) = ecx.memory.get_raw(*alloc_id) {
                if let Some(&(prev_step_count, AllocTracePoint::Changed(ref prev_alloc))) =
                    alloc_trace.trace_points.last()
                {
                    if eq_alloc(alloc, prev_alloc) || *pcx.step_count == prev_step_count {
                        continue;
                    }
                }

                alloc_trace
                    .trace_points
                    .push((*pcx.step_count, AllocTracePoint::Changed(alloc.clone())));
            } else if !matches!(
                alloc_trace.trace_points.last(),
                Some(&(_, AllocTracePoint::Deallocated))
            ) && !alloc_trace.trace_points.is_empty()
            {
                alloc_trace
                    .trace_points
                    .push((*pcx.step_count, AllocTracePoint::Deallocated));
            }
        }
    }

    stack_trace::step_callback(pcx);
}

pub fn routes() -> Vec<::rocket::Route> {
    routes![watch::show, watch::continue_and_show, watch::add]
}

#[get("/show")]
fn show(sender: rocket::State<'_, crate::PrirodaSender>) -> crate::RResult<Html<String>> {
    sender.do_work(|pcx| {
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
                    AllocTracePoint::Changed(alloc) => crate::render::locals::print_alloc(
                        pcx.ecx.memory.pointer_size().bytes(),
                        Pointer::new(*alloc_id, Size::from_bytes(0)).with_tag(miri::Tag::Untagged),
                        alloc,
                        None,
                    ),
                    AllocTracePoint::Deallocated => "Dealloc".to_string(),
                };
                writeln!(
                    buf,
                    "<tr>\n<td>{}</td>\n<td>{}</td>\n</tr>",
                    step_count, content
                )
                .unwrap();
            }
            writeln!(buf, "</table>\n").unwrap();
        }

        Html(buf)
    })
}

#[get("/continue_and_show")]
pub fn continue_and_show(sender: State<'_, PrirodaSender>) -> RResult<Html<String>> {
    sender.do_work(move |pcx| {
        crate::step::step(pcx, |_ecx| crate::step::ShouldContinue::Continue);
    })?;
    show(sender)
}

#[get("/add/<id>")]
pub fn add(
    sender: rocket::State<'_, crate::PrirodaSender>,
    id: u64,
) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
    sender.do_work(move |pcx| {
        rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
            pcx.traces
                .alloc_traces
                .insert(AllocId(id), AllocTrace::new());
            step_callback(pcx);
            "".to_string()
        })
    })
}
