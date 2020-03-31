use std::cell::RefCell;
use std::collections::HashMap;

use miri::{Frame, FrameData, Tag};
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;

use horrorshow::prelude::*;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style, ThemeSet};
use syntect::html::{styled_line_to_highlighted_html, IncludeBackground};
use syntect::parsing::SyntaxSet;
use syntect::util::{split_at, LinesWithEndings};

use self::rent_highlight_cache::*;

lazy_static::lazy_static! {
    static ref SYNTAX_SET: SyntaxSet = SyntaxSet::load_defaults_nonewlines();
    static ref THEME_SET: ThemeSet = ThemeSet::load_defaults();

    static ref RUST_SOURCE: regex::Regex = regex::Regex::new("/rustc/\\w+/").unwrap();
    static ref STD_SRC: Option<String> = {
        if let Ok(output) = std::process::Command::new("rustc").arg("--print").arg("sysroot").output() {
            if let Ok(sysroot) = String::from_utf8(output.stdout) {
                Some(sysroot.trim().to_string() + "/lib/rustlib/src/rust/")
            } else {
                None
            }
        } else {
            None
        }
    };
}

pub fn pretty_src_path(span: Span) -> String {
    let span = format!("{:?}", span);
    let span = RUST_SOURCE.replace(span.as_ref(), "<rust>/").to_string();
    if let Some(std_src) = &*STD_SRC {
        span.replace(std_src, "<rust>/")
    } else {
        span
    }
}

thread_local! {
    // This is a thread local, because a `Span` is only valid within one thread
    static CACHED_HIGHLIGHTED_FILES: RefCell<HashMap<u64, HighlightCacheEntry>> = {
        RefCell::new(HashMap::new())
    };
}

rental! {
    mod rent_highlight_cache {
        use syntect::highlighting::Style;
        #[rental]
        pub struct HighlightCacheEntry {
            string: String,
            highlighted: Vec<(Style, &'string str)>
        }
    }
}

pub fn render_source(
    tcx: TyCtxt,
    frame: Option<&Frame<Tag, FrameData>>,
) -> Box<dyn RenderBox + Send> {
    let before_time = ::std::time::Instant::now();

    if frame.is_none() {
        return Box::new(FnRenderer::new(|_| {}));
    }
    let frame = frame.unwrap();
    let mut instr_spans = if let Some(block) = frame.block {
        if frame.stmt == frame.body[block].statements.len() {
            vec![frame.body[block].terminator().source_info.span]
        } else {
            vec![frame.body[block].statements[frame.stmt]
                .source_info
                .span]
        }
    } else {
        vec![frame.body.span]
    };
    // Get the original macro caller
    while let Some(span) = instr_spans
        .last()
        .unwrap()
        .macro_backtrace()
        .next()
        .map(|b| b.call_site)
    {
        instr_spans.push(span);
    }

    let highlighted_sources = instr_spans
        .into_iter()
        .rev()
        .map(|sp| {
            let (src, lo, hi) = match get_file_source_for_span(tcx, sp) {
                Ok(res) => res,
                Err(err) => return (format!("{:?}", sp), err),
            };

            CACHED_HIGHLIGHTED_FILES.with(|highlight_cache| {
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};

                let mut hasher = DefaultHasher::new();
                src.hash(&mut hasher);
                let hash = hasher.finish();

                highlight_cache
                    .borrow_mut()
                    .entry(hash)
                    .or_insert_with(|| {
                        HighlightCacheEntry::new(src, |src| {
                            let before_time = ::std::time::Instant::now();
                            let highlighted = syntax_highlight(src);
                            let after_time = ::std::time::Instant::now();
                            println!("h: {:?}", after_time - before_time);
                            highlighted
                        })
                    })
                    .rent(|highlighted| (pretty_src_path(sp), mark_span(highlighted, lo, hi)))
            })
        })
        .collect::<Vec<_>>();

    let after_time = ::std::time::Instant::now();
    println!("s: {:?}", after_time - before_time);

    let style = if let Some(bg_color) = THEME_SET.themes["Solarized (dark)"].settings.background {
        format!(
            "background-color: #{:02x}{:02x}{:02x}; display: block;",
            bg_color.r, bg_color.g, bg_color.b
        )
    } else {
        String::new()
    };

    box_html! {
        pre {
            code(id="the_code", style=style) {
                @ for (sp, source) in highlighted_sources {
                    span(style = "color: aqua;") {
                        :sp; br;
                    }
                    : Raw(source);
                    br; br;
                }
            }
        }
    }
}

fn get_file_source_for_span(tcx: TyCtxt, sp: Span) -> Result<(String, usize, usize), String> {
    let source_map = tcx.sess.source_map();
    let _ = source_map.span_to_snippet(sp); // Ensure file src is loaded

    let src = if let Ok(file_lines) = source_map.span_to_lines(sp) {
        if let Some(ref src) = file_lines.file.src {
            src.to_string()
        } else if let Some(src) = file_lines.file.external_src.borrow().get_source() {
            src.to_string()
        } else {
            return Err("<no source info for span>".to_string());
        }
    } else {
        return Err("<couldnt get lines for span>".to_string());
    };
    let lo = source_map.bytepos_to_file_charpos(sp.lo()).0;
    let hi = source_map.bytepos_to_file_charpos(sp.hi()).0;
    Ok((src, lo, hi))
}

fn syntax_highlight<'a, 's>(src: &'s str) -> Vec<(Style, &'s str)> {
    let theme = &THEME_SET.themes["Solarized (dark)"];
    let mut h = HighlightLines::new(
        &SYNTAX_SET
            .find_syntax_by_extension("rs")
            .unwrap()
            .to_owned(),
        theme,
    );

    let mut highlighted = Vec::new();
    for line in LinesWithEndings::from(src) {
        highlighted.extend(h.highlight(line, &SYNTAX_SET));
    }
    highlighted
}

fn mark_span(src: &[(Style, &str)], lo: usize, hi: usize) -> String {
    let (before, with) = split_at(src, lo);
    let (it, after) = split_at(&with, hi - lo);

    let before = styled_line_to_highlighted_html(&before, IncludeBackground::No);
    let it = styled_line_to_highlighted_html(&it, IncludeBackground::No);
    let after = styled_line_to_highlighted_html(&after, IncludeBackground::No);

    if lo == hi {
        assert_eq!(it.len(), 0);
        format!("{}<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>←</span>{}", before, after)
    } else {
        assert_ne!(it.len(), 0);
        format!("{}<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>{}</span>{}", before, it, after)
    }
}
