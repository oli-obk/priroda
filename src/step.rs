use EvalContext;

pub enum ShouldContinue {
    Continue,
    Stop,
}

pub fn step_command(ecx: &mut EvalContext, cmd: &str) -> Option<String> {
    match cmd {
        "step" => {
            Some(step(ecx, |_ecx| ShouldContinue::Stop).unwrap_or_else(||String::new()))
        },
        "next" => {
            let frame = ecx.stack().len();
            let stmt = ecx.stack().last().unwrap().stmt;
            let block = ecx.stack().last().unwrap().block;
            let message = step(ecx, |ecx| {
                if ecx.stack().len() <= frame && (block < ecx.stack().last().unwrap().block || stmt < ecx.stack().last().unwrap().stmt) {
                    ShouldContinue::Stop
                } else {
                    ShouldContinue::Continue
                }
            });
            Some(message.unwrap_or_else(||String::new()))
        },
        "return" => {
            let frame = ecx.stack().len();
            let message = step(ecx, |ecx| if ecx.stack().len() <= frame && is_ret(&ecx) { ShouldContinue::Stop } else { ShouldContinue::Continue });
            Some(message.unwrap_or_else(||String::new()))
        }
        "continue" => {
            let message = step(ecx, |_ecx| ShouldContinue::Continue);
            Some(message.unwrap_or_else(||String::new()))
        },
        _ => None
    }
}

pub fn step<F>(ecx: &mut EvalContext, continue_while: F) -> Option<String>
    where F: Fn(&EvalContext) -> ShouldContinue {
    let mut message = None;
    loop {
        if ecx.stack().len() <= 1 && is_ret(&ecx) {
            break;
        }
        match ecx.step() {
            Ok(true) => {
                if let Some(frame) = ecx.stack().last() {
                    let blck = &frame.mir.basic_blocks()[frame.block];
                    if frame.stmt != blck.statements.len() {
                        if ::should_hide_stmt(&blck.statements[frame.stmt]) {
                            continue;
                        }
                    }
                }
                if let ShouldContinue::Stop = continue_while(&*ecx) {
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
