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

// These are costly to create
lazy_static! {
    static ref BEG_REGEX: Regex = Regex::new(r#"<span style="[\w\d#:;]+">/\*BEG_HIGHLIGHT\*/</span>"#).unwrap();
    static ref END_REGEX: Regex = Regex::new(r#"<span style="[\w\d#:;]+">/\*END_HIGHLIGHT\*/(\s*)</span>"#).unwrap();
    static ref BOTH_REGEX: Regex = Regex::new(r#"<span style="[\w\d#:;]+">/\*BEG_HIGHLIGHT\*//\*END_HIGHLIGHT\*/(\s*)</span>"#).unwrap();
}

pub fn render_source(tcx: TyCtxt, frame: Option<&Frame>) -> Box<RenderBox + Send> {
    if frame.is_none() {
        return Box::new(FnRenderer::new(|_| {}));
    }
    let frame = frame.unwrap();
    let codemap = tcx.sess.codemap();
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

    let ts = ThemeSet::load_defaults();
    let t = &ts.themes["Solarized (light)"];
    let bg_color = t.settings.background.unwrap_or(Color::WHITE);
    let mut h = RUST_SYNTAX.with(|syntax| HighlightLines::new(syntax, t));

    let highlighted_sources = instr_spans.iter().map(|sp| {
        let _ = codemap.span_to_snippet(*sp); // Ensure file src is loaded

        (sp, if let Ok(file_lines) = codemap.span_to_lines(*sp) {
            if let Some(ref src) = file_lines.file.src {
                Ok(src.to_string())
            } else if let Some(src) = file_lines.file.external_src.borrow().get_source() {
                Ok(src.to_string())
            } else {
                Err("<no source info for span>".to_string())
            }
        } else {
            Err("<couldnt get lines for span>".to_string())
        })
    }).map(|(sp, src)| {
        match src {
            Ok(mut file_src) => {
                let lo = codemap.bytepos_to_file_charpos(sp.lo()).0;
                let hi = codemap.bytepos_to_file_charpos(sp.hi()).0;
                file_src.insert_str(hi as usize, "/*END_HIGHLIGHT*/");
                file_src.insert_str(lo as usize, "/*BEG_HIGHLIGHT*/");
                (sp, file_src)
            }
            Err(err) => (sp, err),
        }
    }).map(|(sp, src)| {
        (format!("{:?}", sp), src
            .split('\n')
            .into_iter()
            .map(|l| {
                let highlighted = styles_to_coloured_html(&h.highlight(&l), IncludeBackground::No);
                let highlighted = BEG_REGEX.replace(
                    &highlighted,
                    "<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>",
                );
                let highlighted = END_REGEX.replace(&highlighted, "</span>$1");
                let highlighted = BOTH_REGEX.replace(
                    &highlighted,
                    "<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>←</span>"
                );
                highlighted.into_owned()
            })
            .fold(String::new(), |acc, x| acc + "\n" + &x))
    }).rev().collect::<Vec<_>>();

    box_html! {
        pre {
            code(id="the_code", style=format!("background-color: #{:02x}{:02x}{:02x}; display: block;", bg_color.r, bg_color.g, bg_color.b)) {
                @ for (sp, source) in highlighted_sources {
                    : sp;
                    : Raw(source);
                    br; br;
                }
            }
        }
    }
}
