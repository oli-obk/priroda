use rustc::hir::def_id::DefId;
use rustc::mir;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;

use {EvalContext, PrirodaContext};

pub enum ShouldContinue {
    Continue,
    Stop,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Breakpoint(pub DefId, pub mir::BasicBlock, pub usize);


#[derive(Default)]
pub struct BreakpointTree(HashMap<DefId, HashSet<Breakpoint>>);

impl BreakpointTree {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_breakpoint(&mut self, bp: Breakpoint) {
        self.0.entry(bp.0).or_insert(HashSet::new()).insert(bp);
    }

    pub fn remove_breakpoint(&mut self, bp: Breakpoint) -> bool{
        self.0.get_mut(&bp.0).map(|local|local.remove(&bp)).unwrap_or(false)
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
        self.for_def_id(frame.instance.def_id()).breakpoint_exists(frame.block, frame.stmt)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Breakpoint> {
        self.0.values().flat_map(|local| {
            local.iter()
        })
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
            LocalBreakpoints::SomeBps(bps) => bps.iter().any(|bp| {
                bp.1 == bb && bp.2 == stmt
            })
        }
    }
}

pub fn step_command(pcx: &mut PrirodaContext, cmd: &str) -> Option<String> {
    match cmd {
        "step" => {
            Some(step(pcx, |_ecx| ShouldContinue::Stop).unwrap_or_else(||String::new()))
        },
        "next" => {
            let frame = pcx.stack().len();
            let stmt = pcx.frame().stmt;
            let block = pcx.frame().block;
            let message = step(pcx, |ecx| {
                if ecx.stack().len() <= frame && (block < ecx.frame().block || stmt < ecx.frame().stmt) {
                    ShouldContinue::Stop
                } else {
                    ShouldContinue::Continue
                }
            });
            Some(message.unwrap_or_else(||String::new()))
        },
        "return" => {
            let frame = pcx.stack().len();
            let message = step(pcx, |ecx| {
                if ecx.stack().len() <= frame && is_ret(&ecx) {
                    ShouldContinue::Stop
                } else {
                    ShouldContinue::Continue
                }
            });
            Some(message.unwrap_or_else(||String::new()))
        }
        "continue" => {
            let message = step(pcx, |_ecx| ShouldContinue::Continue);
            Some(message.unwrap_or_else(||String::new()))
        },
        _ => None
    }
}

pub fn step<F>(pcx: &mut PrirodaContext, continue_while: F) -> Option<String>
    where F: Fn(&EvalContext) -> ShouldContinue {
    let mut message = None;
    loop {
        if pcx.stack().len() <= 1 && is_ret(&*pcx) {
            break;
        }
        match pcx.step() {
            Ok(true) => {
                if let Some(frame) = pcx.stack().last() {
                    let blck = &frame.mir.basic_blocks()[frame.block];
                    if frame.stmt != blck.statements.len() {
                        if ::should_hide_stmt(&blck.statements[frame.stmt]) && !pcx.bptree.is_at_breakpoint(&*pcx) {
                            continue;
                        }
                    }
                }
                if let ShouldContinue::Stop = continue_while(&*pcx) {
                    break;
                }
                if pcx.bptree.is_at_breakpoint(pcx) {
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
    message
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
