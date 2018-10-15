use std::cell::RefCell;
use std::collections::HashMap;

use miri::Frame;
use rustc::ty::TyCtxt;
use syntax::codemap::Span;

use horrorshow::prelude::*;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Color, Style, ThemeSet};
use syntect::html::{styles_to_coloured_html, IncludeBackground};
use syntect::parsing::{SyntaxDefinition, SyntaxSet};

use self::rent_highlight_cache::*;

thread_local! {
    // Loading the syntax every time is very heavy and causes noticable slowdown
    // This is a thread local as a `SyntaxDefinition` is neither `Send` nor `Sync`
    static RUST_SYNTAX: SyntaxDefinition = {
        let ps = SyntaxSet::load_defaults_nonewlines();
        ps.find_syntax_by_extension("rs").unwrap().to_owned()
    };

    static THEME_SET: ThemeSet = {
        ThemeSet::load_defaults()
    };

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

// Split between i and i+1
fn split<'s, A: Clone>(
    mut s: &[(A, &'s str)],
    mut i: usize,
) -> (Vec<(A, &'s str)>, Vec<(A, &'s str)>) {
    let mut before = Vec::new();
    let mut after = Vec::new();

    while s.len() > 0 && s[0].1.len() < i {
        i -= s[0].1.len();
        before.push(s[0].clone());
        s = &s[1..];
    }

    if s.len() > 0 {
        let (data, str) = s[0].clone();
        s = &s[1..];
        if i != 0 {
            before.push((data.clone(), &str[..i]));
        }
        if i != str.len() {
            after.push((data, &str[i..]));
        }
    }

    after.extend(s.iter().cloned());
    (before, after)
}

pub fn render_source(tcx: TyCtxt, frame: Option<&Frame>) -> Box<RenderBox + Send> {
    let before_time = ::std::time::Instant::now();

    if frame.is_none() {
        return Box::new(FnRenderer::new(|_| {}));
    }
    let frame = frame.unwrap();
    let mut instr_spans = vec![if frame.stmt == frame.mir[frame.block].statements.len() {
        frame.mir[frame.block].terminator().source_info.span
    } else {
        frame.mir[frame.block].statements[frame.stmt]
            .source_info
            .span
    }];
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

    let (bg_color, highlighted_sources) = get_highlighter(|mut h| {
        let highlighted_sources = instr_spans
            .iter()
            .rev()
            .map(|sp| {
                let (src, lo, hi) = match get_file_source_for_span(tcx, *sp) {
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
                                let highlighted = h.highlight(src);
                                let after_time = ::std::time::Instant::now();
                                println!("h: {:?}", after_time - before_time);
                                highlighted
                            })
                        })
                        .rent(|highlighted| (format!("{:?}", sp), mark_span(highlighted, lo, hi)))
                })
            })
            .collect::<Vec<_>>();

        highlighted_sources
    });

    let after_time = ::std::time::Instant::now();
    println!("s: {:?}", after_time - before_time);

    box_html! {
        pre {
            code(id="the_code", style=format!("background-color: #{:02x}{:02x}{:02x}; display: block;", bg_color.r, bg_color.g, bg_color.b)) {
                @ for (sp, source) in highlighted_sources {
                    : sp; br;
                    : Raw(source);
                    br; br;
                }
            }
        }
    }
}

fn get_highlighter<T>(f: impl FnOnce(HighlightLines) -> T) -> (Color, T) {
    THEME_SET.with(|ts| {
        let t = &ts.themes["Solarized (light)"];
        let bg_color = t.settings.background.unwrap_or(Color::WHITE);
        let h = RUST_SYNTAX.with(|syntax| HighlightLines::new(syntax, t));
        (bg_color, f(h))
    })
}

fn get_file_source_for_span(tcx: TyCtxt, sp: Span) -> Result<(String, usize, usize), String> {
    let codemap = tcx.sess.codemap();
    let _ = codemap.span_to_snippet(sp); // Ensure file src is loaded

    let src = if let Ok(file_lines) = codemap.span_to_lines(sp) {
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
    let lo = codemap.bytepos_to_file_charpos(sp.lo()).0;
    let hi = codemap.bytepos_to_file_charpos(sp.hi()).0;
    Ok((src, lo, hi))
}

fn mark_span(src: &[(Style, &str)], lo: usize, hi: usize) -> String {
    let before_time = ::std::time::Instant::now();
    let (before, with) = split(src, lo);
    let (it, after) = split(&with, hi - lo);
    let after_time = ::std::time::Instant::now();
    println!("m: {:?}", after_time - before_time);

    let before = styles_to_coloured_html(&before, IncludeBackground::No);
    let it = styles_to_coloured_html(&it, IncludeBackground::No);
    let after = styles_to_coloured_html(&after, IncludeBackground::No);

    if lo == hi {
        assert_eq!(it.len(), 0);
        format!("{}<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>←</span>{}", before, after)
    } else {
        assert_ne!(it.len(), 0);
        format!("{}<span style='background-color: lightcoral; border-radius: 5px; padding: 1px;'>{}</span>{}", before, it, after)
    }
}
