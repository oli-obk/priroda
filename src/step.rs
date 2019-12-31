use rustc::hir::def_id::{CrateNum, DefId, DefIndex};
use rustc::mir;
use rustc_index::vec::Idx;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

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

    pub fn for_def_id(&self, def_id: DefId) -> LocalBreakpoints {
        if let Some(bps) = self.0.get(&def_id) {
            LocalBreakpoints::SomeBps(bps)
        } else {
            LocalBreakpoints::NoBp
        }
    }

    pub fn is_at_breakpoint(&self, ecx: &InterpCx) -> bool {
        let frame = ecx.frame();
        self.for_def_id(frame.instance.def_id())
            .breakpoint_exists(frame.block, frame.stmt)
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
    pub fn breakpoint_exists(self, bb: Option<mir::BasicBlock>, stmt: usize) -> bool {
        if let Some(bb) = bb {
            match self {
                LocalBreakpoints::NoBp => false,
                LocalBreakpoints::SomeBps(bps) => bps.iter().any(|bp| bp.1 == bb && bp.2 == stmt),
            }
        } else {
            // Unwinding, but no cleanup for this frame
            // FIXME make this configurable
            true
        }
    }
}

pub fn step<F>(pcx: &mut PrirodaContext, continue_while: F) -> String
where
    F: Fn(&InterpCx) -> ShouldContinue,
{
    let mut message = None;
    loop {
        if pcx.ecx.stack().len() <= 1 && is_ret(&pcx.ecx) {
            break;
        }
        match pcx.ecx.step() {
            Ok(true) => {
                *pcx.step_count += 1;
                crate::watch::step_callback(pcx);

                if let Some(frame) = pcx.ecx.stack().last() {
                    if let Some(block) = frame.block {
                        let blck = &frame.body.basic_blocks()[block];
                        if frame.stmt != blck.statements.len()
                            && crate::should_hide_stmt(&blck.statements[frame.stmt])
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
            Ok(false) => {
                message = Some("interpretation finished".to_string());
                break;
            }
            Err(e) => {
                message = Some(format!("{:?}", e));
                break;
            }
        }
    }
    message.unwrap_or_else(String::new)
}

pub fn is_ret(ecx: &InterpCx) -> bool {
    if let Some(frame) = ecx.stack().last() {
        if let Some(block) = frame.block {
            let basic_block = &frame.body.basic_blocks()[block];

            match basic_block.terminator().kind {
                rustc::mir::TerminatorKind::Return | rustc::mir::TerminatorKind::Resume => {
                    frame.stmt >= basic_block.statements.len()
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
    use crate::action_route;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![restart, single, single_back, next, return_, continue_]
    }

    action_route!(restart: "/restart", |pcx| {
        pcx.restart();
        "restarted".to_string()
    });

    action_route!(single: "/single", |pcx| {
        step(pcx, |_ecx| ShouldContinue::Stop)
    });

    action_route!(single_back: "/single_back", |pcx| {
        let orig_step_count = *pcx.step_count;
        pcx.restart();
        *pcx.step_count = orig_step_count;
        if *pcx.step_count > 0 {
            *pcx.step_count -= 1;
            for _ in 0..*pcx.step_count {
                match pcx.ecx.step() {
                    Ok(true) => crate::watch::step_callback(pcx), // Rebuild traces till the current instruction
                    res => return format!("Miri is not deterministic causing error {:?}", res),
                }
            }
            "stepped back".to_string()
        } else {
            "already at the start".to_string()
        }
    });

    action_route!(next: "/next", |pcx| {
        let frame = pcx.ecx.stack().len();
        let stmt = pcx.ecx.frame().stmt;
        let block = pcx.ecx.frame().block;
        step(pcx, |ecx| {
            if ecx.stack().len() <= frame && (block < ecx.frame().block || stmt < ecx.frame().stmt) {
                ShouldContinue::Stop
            } else {
                ShouldContinue::Continue
            }
        })
    });

    action_route!(return_: "/return", |pcx| {
        let frame = pcx.ecx.stack().len();
        step(pcx, |ecx| {
            if ecx.stack().len() <= frame && is_ret(&ecx) {
                ShouldContinue::Stop
            } else {
                ShouldContinue::Continue
            }
        })
    });

    action_route!(continue_: "/continue", |pcx| {
        step(pcx, |_ecx| ShouldContinue::Continue)
    });
}

pub mod bp_routes {
    use super::*;
    use crate::action_route;
    use std::path::PathBuf;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![add_here, add, remove, remove_all]
    }

    action_route!(add_here: "/add_here", |pcx| {
        let frame = pcx.ecx.frame();
        if let Some(block) = frame.block {
            pcx.config.bptree.add_breakpoint(Breakpoint(frame.instance.def_id(), block, frame.stmt));
            format!("Breakpoint added for {:?}@{}:{}", frame.instance.def_id(), block.index(), frame.stmt)
        } else {
            format!("Can't set breakpoint for unwinding without cleanup yet")
        }
    });

    action_route!(add: "/add/<path..>", |pcx, path: PathBuf| {
        let path = path.to_string_lossy();
        let res = parse_breakpoint_from_url(&path);
        match res {
            Ok(breakpoint) => {
                pcx.config.bptree.add_breakpoint(breakpoint);
                format!("Breakpoint added for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
            }
            Err(e) => e,
        }
    });

    action_route!(remove: "/remove/<path..>", |pcx, path: PathBuf| {
        let path = path.to_string_lossy();
        let res = parse_breakpoint_from_url(&path);
        match res {
            Ok(breakpoint) => {
                if pcx.config.bptree.remove_breakpoint(breakpoint) {
                    format!("Breakpoint removed for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
                } else {
                    format!("No breakpoint for for {:?}@{}:{}", breakpoint.0, breakpoint.1.index(), breakpoint.2)
                }
            }
            Err(e) => e,
        }
    });

    action_route!(remove_all: "/remove_all", |pcx| {
        pcx.config.bptree.remove_all();
        "All breakpoints removed".to_string()
    });
}
