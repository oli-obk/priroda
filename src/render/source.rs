use rustc::ty::TyCtxt;
use miri::Frame;
use syntect::easy::HighlightLines;
use syntect::parsing::{SyntaxDefinition, SyntaxSet};
use syntect::highlighting::{Color, ThemeSet};
use syntect::html::{styles_to_coloured_html, IncludeBackground};
use horrorshow::prelude::*;
use regex::Regex;

thread_local! {
    /// Loading the syntax every time is very heavy and causes noticable slowdown
    /// This is a thread local as a `SyntaxDefinition` is neither `Send` nor `Sync`
    static RUST_SYNTAX: SyntaxDefinition = {
        let ps = SyntaxSet::load_defaults_nonewlines();
        ps.find_syntax_by_extension("rs").unwrap().to_owned()
    };
}

pub fn render_source(tcx: TyCtxt, frame: Option<&Frame>) -> Box<RenderBox + Send> {
    if frame.is_none() {
        return Box::new(FnRenderer::new(|_| {}));
    }
    let frame = frame.unwrap();
    let codemap = tcx.sess.codemap();
    let mir_span = frame.mir.span;
    let mut instr_spans = vec![
        if frame.stmt == frame.mir[frame.block].statements.len() {
            frame.mir[frame.block].terminator().source_info.span
        } else {
            frame.mir[frame.block].statements[frame.stmt]
                .source_info
                .span
        },
    ];
    // Get the original macro caller
    while let Some(span) = instr_spans
        .last()
        .unwrap()
        .macro_backtrace()
        .get(0)
        .map(|b| b.call_site)
    {
        instr_spans.push(span);
    }

    let instr_bytepos_begin_end = (
        instr_spans.last().unwrap().lo(),
        instr_spans.last().unwrap().hi(),
    );
    let mut mir_src = if let Ok(file_lines) = codemap.span_to_lines(mir_span) {
        if let Some(ref src) = file_lines.file.src {
            Ok(src.to_string())
        } else {
            Err("<no source info for span>".to_string())
        }
    } else {
        Err("<couldnt get lines for span>".to_string())
    };
    if let Ok(ref mut mir_src) = mir_src {
        mir_src.insert_str((instr_bytepos_begin_end.1).0 as usize, "/*END_HIGHLIGHT*/");
        mir_src.insert_str((instr_bytepos_begin_end.0).0 as usize, "/*BEG_HIGHLIGHT*/");
    }
    let mir_lines = mir_src
        .unwrap_or_else(|e| e)
        .split('\n')
        .map(|l| l.to_string())
        .collect::<Vec<_>>();

    let macro_backtrace = format!("macro backtrace: {:#?}", instr_spans);
    box_html! {
        pre {
            div(id="macro_backtrace") {
                : macro_backtrace
            }
            br;
            |tmpl| {
                let ts = ThemeSet::load_defaults();
                let t = &ts.themes["Solarized (light)"];
                let c = t.settings.background.unwrap_or(Color::WHITE);
                let mut h = RUST_SYNTAX.with(|syntax| { HighlightLines::new(syntax, t) });
                tmpl << html! {
                    code(id="the_code", style=format!("background-color: #{:02x}{:02x}{:02x}; display: block;", c.r, c.g, c.b)) {
                        @ for line in mir_lines.into_iter().map(|l| {
                            let b_regex = Regex::new(r#"<span style="[\w\d#:;]+">/\*BEG_HIGHLIGHT\*/</span>"#).unwrap();
                            let e_regex = Regex::new(r#"<span style="[\w\d#:;]+">/\*END_HIGHLIGHT\*/(\s*)</span>"#).unwrap();
                            let highlighted = styles_to_coloured_html(&h.highlight(&l), IncludeBackground::No);
                            let highlighted = b_regex.replace(&highlighted, "<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>");
                            let highlighted = e_regex.replace(&highlighted, "</span>$1");
                            highlighted.into_owned()
                        }) {
                            : Raw(line);
                            br;
                        }
                    }
                }
            }
        }
    }
}
