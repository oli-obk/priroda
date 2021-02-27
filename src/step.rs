use rustc_hir::def_id::{CrateNum, DefId, DefIndex};
use rustc_index::vec::Idx;
use rustc_middle::mir;
use rustc_mir::interpret::Machine;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use miri::*;
use serde::de::{Deserialize, Deserializer, Error as SerdeError};

use crate::{InterpCx, PrirodaContext};

pub enum ShouldContinue {
    Continue,
    Stop,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Breakpoint(pub DefId, pub mir::BasicBlock, pub usize);

#[derive(Default)]
pub struct BreakpointTree(HashMap<DefId, HashSet<Breakpoint>>);

impl<'de> Deserialize<'de> for BreakpointTree {
    fn deserialize<D: Deserializer<'de>>(deser: D) -> Result<Self, D::Error> {
        let mut map = HashMap::new();
        for (k, v) in HashMap::<String, HashSet<(usize, usize)>>::deserialize(deser)? {
            let def_id = parse_def_id(&k).map_err(SerdeError::custom)?;
            map.insert(
                def_id,
                v.into_iter()
                    .map(|(bb, instr)| Breakpoint(def_id, mir::BasicBlock::new(bb), instr))
                    .collect::<HashSet<Breakpoint>>(),
            );
        }
        Ok(BreakpointTree(map))
    }
}

impl BreakpointTree {
    pub fn add_breakpoint(&mut self, bp: Breakpoint) {
        self.0.entry(bp.0).or_insert_with(HashSet::new).insert(bp);
    }

    pub fn remove_breakpoint(&mut self, bp: Breakpoint) -> bool {
        self.0
            .get_mut(&bp.0)
            .map(|local| local.remove(&bp))
            .unwrap_or(false)
    }

    pub fn remove_all(&mut self) {
        self.0.clear();
    }

    pub fn for_def_id(&self, def_id: DefId) -> LocalBreakpoints<'_> {
        if let Some(bps) = self.0.get(&def_id) {
            LocalBreakpoints::SomeBps(bps)
        } else {
            LocalBreakpoints::NoBp
        }
    }

    pub fn is_at_breakpoint(&self, ecx: &InterpCx<'_>) -> bool {
        let frame = ecx.frame();
        self.for_def_id(frame.instance.def_id())
            .breakpoint_exists(frame.current_loc().ok())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Breakpoint> {
        self.0.values().flat_map(|local| local.iter())
    }
}

#[derive(Copy, Clone)]
pub enum LocalBreakpoints<'a> {
    NoBp,
    SomeBps(&'a HashSet<Breakpoint>),
}

impl<'a> LocalBreakpoints<'a> {
    pub fn breakpoint_exists(self, location: Option<mir::Location>) -> bool {
        if let Some(location) = location {
            match self {
                LocalBreakpoints::NoBp => false,
                LocalBreakpoints::SomeBps(bps) => bps
                    .iter()
                    .any(|bp| bp.1 == location.block && bp.2 == location.statement_index),
            }
        } else {
            // Unwinding, but no cleanup for this frame
            // FIXME make this configurable
            false
        }
    }
}

pub fn step<F>(pcx: &mut PrirodaContext<'_, '_>, continue_while: F) -> String
where
    F: Fn(&InterpCx<'_>) -> ShouldContinue,
{
    let mut message = None;
    let ret: InterpResult<'_, _> = try {
        loop {
            let is_main_thread_active = pcx.ecx.get_active_thread() == 0.into();
            if is_main_thread_active && Machine::stack(&pcx.ecx).len() <= 1 && is_ret(&pcx.ecx) {
                // When the main thread exists, the program terminates. However,
                // we want to prevent stepping out of the program.
                break;
            }
            match pcx.ecx.schedule()? {
                SchedulingAction::ExecuteStep => {}
                SchedulingAction::ExecuteTimeoutCallback => {
                    pcx.ecx.run_timeout_callback()?;
                    continue;
                }
                SchedulingAction::ExecuteDtors => {
                    // This will either enable the thread again (so we go back
                    // to `ExecuteStep`), or determine that this thread is done
                    // for good.
                    pcx.ecx.schedule_next_tls_dtor_for_active_thread()?;
                    continue;
                }
                SchedulingAction::Stop => {
                    message = Some("interpretation finished".to_string());
                    break;
                }
            }
            let info = pcx.ecx.preprocess_diagnostics();
            if !pcx.ecx.step()? {
                message = Some("a terminated thread was scheduled for execution".to_string());
                break;
            }
            pcx.ecx.process_diagnostics(info);

            *pcx.step_count += 1;
            crate::watch::step_callback(pcx);

            if let Some(frame) = Machine::stack(&pcx.ecx).last() {
                if let Some(location) = frame.current_loc().ok() {
                    let blck = &frame.body.basic_blocks()[location.block];
                    if location.statement_index != blck.statements.len()
                        && crate::should_hide_stmt(&blck.statements[location.statement_index])
                        && !pcx.config.bptree.is_at_breakpoint(&pcx.ecx)
                    {
                        continue;
                    }
                } else {
                    // Unwinding, but no cleanup for this frame
                    // FIXME make step behaviour configurable
                }
            }
            if let ShouldContinue::Stop = continue_while(&pcx.ecx) {
                break;
            }
            if pcx.config.bptree.is_at_breakpoint(&pcx.ecx) {
                break;
            }
        }
    };
    match ret {
        Err(e) => {
            message = Some(format!("{:?}", e));
        }
        Ok(_) => {}
    }
    message.unwrap_or_else(String::new)
}

pub fn is_ret(ecx: &InterpCx<'_>) -> bool {
    if let Some(frame) = Machine::stack(&ecx).last() {
        if let Some(location) = frame.current_loc().ok() {
            let basic_block = &frame.body.basic_blocks()[location.block];

            match basic_block.terminator().kind {
                rustc_middle::mir::TerminatorKind::Return
                | rustc_middle::mir::TerminatorKind::Resume => {
                    location.statement_index >= basic_block.statements.len()
                }
                _ => false,
            }
        } else {
            true // Unwinding, but no cleanup necessary for current frame
        }
    } else {
        true
    }
}

fn parse_breakpoint_from_url(s: &str) -> Result<Breakpoint, String> {
    let regex = ::regex::Regex::new(r#"([^@]+)@(\d+):(\d+)"#).unwrap();
    // DefId(1:14824 ~ mycrate::main)@1:3
    //       ^ ^                      ^ ^
    //       | |                      | statement
    //       | |                      BasicBlock
    //       | DefIndex::as_array_index()
    //       CrateNum

    let s = s.replace("%20", " ");
    let caps = regex
        .captures(&s)
        .ok_or_else(|| format!("Invalid breakpoint {}", s))?;

    // Parse DefId
    let def_id = parse_def_id(caps.get(1).unwrap().as_str())?;

    // Parse block and stmt
    let bb = mir::BasicBlock::new(
        caps.get(2)
            .unwrap()
            .as_str()
            .parse::<usize>()
            .map_err(|_| "block id is not a positive integer")?,
    );
    let stmt = caps
        .get(3)
        .unwrap()
        .as_str()
        .parse::<usize>()
        .map_err(|_| "stmt id is not a positive integer")?;

    Ok(Breakpoint(def_id, bb, stmt))
}

fn parse_def_id(s: &str) -> Result<DefId, String> {
    let regex = ::regex::Regex::new(r#"DefId\((\d+):(\d+) ~ [^\)]+\)"#).unwrap();
    let caps = regex
        .captures(&s)
        .ok_or_else(|| format!("Invalid def_id {}", s))?;

    let crate_num = CrateNum::new(caps.get(1).unwrap().as_str().parse::<usize>().unwrap());
    let index = caps
        .get(2)
        .unwrap()
        .as_str()
        .parse::<usize>()
        .map_err(|_| "index is not a positive integer".to_string())?;
    let def_index = DefIndex::from_usize(index);
    Ok(DefId {
        krate: crate_num,
        index: def_index,
    })
}

pub mod step_routes {
    use super::*;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![restart, single, single_back, next, return_, continue_]
    }

    #[get("/restart")]
    fn restart(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                pcx.restart();
                "restarted".to_string()
            })
        })
    }

    #[get("/single")]
    fn single(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                step(pcx, |_ecx| ShouldContinue::Stop)
            })
        })
    }
    #[get("/single_back")]
    fn single_back(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(
                rocket::response::Redirect::to("/"),
                (|| {
                    let orig_step_count = *pcx.step_count;
                    pcx.restart();
                    *pcx.step_count = orig_step_count;
                    if *pcx.step_count > 0 {
                        *pcx.step_count -= 1;
                        for _ in 0..*pcx.step_count {
                            match pcx.ecx.step() {
                                Ok(true) => crate::watch::step_callback(pcx), // Rebuild traces till the current instruction
                                res => {
                                    return format!(
                                        "Miri is not deterministic causing error {:?}",
                                        res
                                    )
                                }
                            }
                        }
                        "stepped back".to_string()
                    } else {
                        "already at the start".to_string()
                    }
                })(),
            )
        })
    }

    #[get("/next")]
    fn next(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                let frame = Machine::stack(&pcx.ecx).len();
                let (block, stmt): (Option<mir::BasicBlock>, _) =
                    if let Some(location) = pcx.ecx.frame().current_loc().ok() {
                        (Some(location.block), Some(location.statement_index))
                    } else {
                        (None, None)
                    };
                step(pcx, move |ecx| {
                    let (curr_block, curr_stmt) =
                        if let Some(location) = ecx.frame().current_loc().ok() {
                            (Some(location.block), Some(location.statement_index))
                        } else {
                            (None, None)
                        };
                    if Machine::stack(&ecx).len() <= frame
                        && (block < curr_block || stmt < curr_stmt)
                    {
                        ShouldContinue::Stop
                    } else {
                        ShouldContinue::Continue
                    }
                })
            })
        })
    }

    #[get("/return")]
    fn return_(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                let frame = Machine::stack(&pcx.ecx).len();
                step(pcx, |ecx| {
                    if Machine::stack(&ecx).len() <= frame && is_ret(&ecx) {
                        ShouldContinue::Stop
                    } else {
                        ShouldContinue::Continue
                    }
                })
            })
        })
    }

    #[get("/continue")]
    fn continue_(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                step(pcx, |_ecx| ShouldContinue::Continue)
            })
        })
    }
}

pub mod bp_routes {
    use super::*;
    use std::path::PathBuf;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![add_here, add, remove, remove_all]
    }

    #[get("/add_here")]
    pub fn add_here(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                let frame = pcx.ecx.frame();
                if let Some(location) = frame.current_loc().ok() {
                    pcx.config.bptree.add_breakpoint(Breakpoint(
                        frame.instance.def_id(),
                        location.block,
                        location.statement_index,
                    ));
                    format!(
                        "Breakpoint added for {:?}@{}:{}",
                        frame.instance.def_id(),
                        location.block.index(),
                        location.statement_index
                    )
                } else {
                    format!("Can't set breakpoint for unwinding without cleanup yet")
                }
            })
        })
    }

    #[get("/add/<path..>")]
    pub fn add(
        sender: rocket::State<'_, crate::PrirodaSender>,
        path: PathBuf,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                let path = path.to_string_lossy();
                let res = parse_breakpoint_from_url(&path);
                match res {
                    Ok(breakpoint) => {
                        pcx.config.bptree.add_breakpoint(breakpoint);
                        format!(
                            "Breakpoint added for {:?}@{}:{}",
                            breakpoint.0,
                            breakpoint.1.index(),
                            breakpoint.2
                        )
                    }
                    Err(e) => e,
                }
            })
        })
    }

    #[get("/remove/<path..>")]
    pub fn remove(
        sender: rocket::State<'_, crate::PrirodaSender>,
        path: PathBuf,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                let path = path.to_string_lossy();
                let res = parse_breakpoint_from_url(&path);
                match res {
                    Ok(breakpoint) => {
                        if pcx.config.bptree.remove_breakpoint(breakpoint) {
                            format!(
                                "Breakpoint removed for {:?}@{}:{}",
                                breakpoint.0,
                                breakpoint.1.index(),
                                breakpoint.2
                            )
                        } else {
                            format!(
                                "No breakpoint for for {:?}@{}:{}",
                                breakpoint.0,
                                breakpoint.1.index(),
                                breakpoint.2
                            )
                        }
                    }
                    Err(e) => e,
                }
            })
        })
    }

    #[get("/remove_all")]
    pub fn remove_all(
        sender: rocket::State<'_, crate::PrirodaSender>,
    ) -> crate::RResult<rocket::response::Flash<rocket::response::Redirect>> {
        sender.do_work(move |pcx| {
            rocket::response::Flash::success(rocket::response::Redirect::to("/"), {
                pcx.config.bptree.remove_all();
                "All breakpoints removed".to_string()
            })
        })
    }
}
