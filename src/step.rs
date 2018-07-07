use rustc::hir::def_id::{CrateNum, DefId, DefIndex, DefIndexAddressSpace};
use rustc::mir;
use rustc_data_structures::indexed_vec::Idx;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use serde::de::{Deserialize, Deserializer, Error as SerdeError};

use {EvalContext, PrirodaContext};

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
        for (k, v) in HashMap::<String, HashSet<(usize, usize)>>::deserialize(deser)?.into_iter() {
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
        self.0.entry(bp.0).or_insert(HashSet::new()).insert(bp);
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

    pub fn is_at_breakpoint(&self, ecx: &EvalContext) -> bool {
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
    pub fn breakpoint_exists(&self, bb: mir::BasicBlock, stmt: usize) -> bool {
        match *self {
            LocalBreakpoints::NoBp => false,
            LocalBreakpoints::SomeBps(bps) => bps.iter().any(|bp| bp.1 == bb && bp.2 == stmt),
        }
    }
}

pub fn step<F>(pcx: &mut PrirodaContext, continue_while: F) -> String
where
    F: Fn(&EvalContext) -> ShouldContinue,
{
    let mut message = None;
    loop {
        if pcx.ecx.stack().len() <= 1 && is_ret(&pcx.ecx) {
            break;
        }
        match pcx.ecx.step() {
            Ok(true) => {
                *pcx.step_count += 1;
                ::watch::step_callback(pcx);

                if let Some(frame) = pcx.ecx.stack().last() {
                    let blck = &frame.mir.basic_blocks()[frame.block];
                    if frame.stmt != blck.statements.len() {
                        if ::should_hide_stmt(&blck.statements[frame.stmt])
                            && !pcx.config.bptree.is_at_breakpoint(&pcx.ecx)
                        {
                            continue;
                        }
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

pub fn is_ret(ecx: &EvalContext) -> bool {
    if let Some(stack) = ecx.stack().last() {
        let basic_block = &stack.mir.basic_blocks()[stack.block];

        match basic_block.terminator().kind {
            ::rustc::mir::TerminatorKind::Return => stack.stmt >= basic_block.statements.len(),
            _ => false,
        }
    } else {
        true
    }
}

fn parse_breakpoint_from_url(s: &str) -> Result<Breakpoint, String> {
    let regex = ::regex::Regex::new(r#"([^@]+)@(\d+):(\d+)"#).unwrap();
    // DefId(1/0:14824 ~ mycrate::main)@1:3
    //       ^ ^ ^                      ^ ^
    //       | | |                      | statement
    //       | | |                      BasicBlock
    //       | | DefIndex::as_array_index()
    //       | DefIndexAddressSpace
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
    let regex = ::regex::Regex::new(r#"DefId\((\d+)/(0|1):(\d+) ~ [^\)]+\)"#).unwrap();
    let caps = regex
        .captures(&s)
        .ok_or_else(|| format!("Invalid def_id {}", s))?;

    let crate_num = CrateNum::new(caps.get(1).unwrap().as_str().parse::<usize>().unwrap());
    let address_space = match caps.get(2).unwrap().as_str().parse::<u64>().unwrap() {
        0 => DefIndexAddressSpace::Low,
        1 => DefIndexAddressSpace::High,
        _ => return Err("address_space is not 0 or 1".to_string()),
    };
    let index = caps
        .get(3)
        .unwrap()
        .as_str()
        .parse::<usize>()
        .map_err(|_| "index is not a positive integer".to_string())?;
    let def_index = DefIndex::from_array_index(index, address_space);
    Ok(DefId {
        krate: crate_num,
        index: def_index,
    })
}

pub mod step_routes {
    use super::*;
    use action_route;

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
        pcx.restart();
        if *pcx.step_count > 0 {
            *pcx.step_count -= 1;
            for _ in 0..*pcx.step_count {
                match pcx.ecx.step() {
                    Ok(true) => ::watch::step_callback(pcx), // Rebuild traces till the current instruction
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
    use action_route;
    use std::path::PathBuf;

    pub fn routes() -> Vec<::rocket::Route> {
        routes![add_here, add, remove, remove_all]
    }

    action_route!(add_here: "/add_here", |pcx| {
        let frame = pcx.ecx.frame();
        pcx.config.bptree.add_breakpoint(Breakpoint(frame.instance.def_id(), frame.block, frame.stmt));
        format!("Breakpoint added for {:?}@{}:{}", frame.instance.def_id(), frame.block.index(), frame.stmt)
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
