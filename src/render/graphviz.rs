// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use dot;
use step::LocalBreakpoints;
use rustc::mir::*;
use std::fmt::{self, Debug, Write};
use miri::Frame;

use rustc_data_structures::indexed_vec::Idx;

pub fn render_html(frame: &Frame, breakpoints: LocalBreakpoints) -> String {
    let mut rendered = String::new();
    render_mir_svg(&frame.mir, breakpoints, &mut rendered, None).unwrap();
    for (i, promoted) in frame.mir.promoted.iter_enumerated() {
        println!("promoted: {:?}", i);
        render_mir_svg(promoted, breakpoints, &mut rendered, Some(i.index())).unwrap();
    }
    let (bb, stmt) = {
        let blck = &frame.mir.basic_blocks()[frame.block];
        use rustc_data_structures::indexed_vec::Idx;
        (
            frame.block.index() + 1,
            if frame.stmt == blck.statements.len() {
                if blck.statements.is_empty() {
                    6
                } else {
                    blck.statements.len() + 7
                }
            } else {
                assert!(frame.stmt < blck.statements.len());
                frame.stmt + 6
            },
        )
    };
    let edge_colors = {
        let blck = &frame.mir.basic_blocks()[frame.block];
        let (targets, unwind) = if frame.stmt == blck.statements.len() {
            use rustc::mir::TerminatorKind::*;
            match blck.terminator().kind {
                Goto { target } => (vec![target], None),
                SwitchInt { ref targets, .. } => (targets.to_vec(), None),
                Drop { target, unwind, .. } |
                DropAndReplace { target, unwind, .. } => (vec![target], unwind),
                Call { ref destination, cleanup, .. } => {
                    if let Some((_, target)) = *destination {
                        (vec![target], cleanup)
                    } else {
                        (vec![], cleanup)
                    }
                }
                _ => (vec![], None),
            }
        } else {
            (vec![], None)
        };
        format!("let edge_colors = {{{}}};",
            targets.into_iter().map(|target|(frame.block, target, "green"))
                .chain(unwind.into_iter().map(|target|(frame.block, target, "red")))
                .map(|(from, to, color)| format!("'bb{}->bb{}':'{}'", from.index(), to.index(), color))
                .collect::<Vec<_>>()
                .join(",")
        )
    };
    rendered.write_fmt(format_args!(r##"<style>
        #node{} > text:nth-child({}) {{
            fill: red;
        }}
        .edge-green > path, .edge-green > polygon, .edge-green > text {{
            fill: green;
            stroke: green;
        }}
        .edge-red > path, .edge-red > polygon, .edge-red > text {{
            fill: red;
            stroke: red;
        }}
        .edge > path {{
            fill: none;
        }}
        </style>
        <script>
        {edge_colors}
        for(let el of document.querySelectorAll("#mir > svg #graph0 .edge")) {{
            let title = el.querySelector("title").textContent;
            if(title in edge_colors) {{
                el.classList.add("edge-" + edge_colors[title]);
            }}
        }}
        </script>"##, bb, stmt, edge_colors = edge_colors)).unwrap();
    rendered
}

/// Write a graphviz DOT graph of a list of MIRs.
pub fn render_mir_svg<W: Write>(mir: &Mir, breakpoints: LocalBreakpoints, w: &mut W, promoted: Option<usize>) -> fmt::Result {
    let mut dot = String::new();
    if let Some(promoted) = promoted {
        writeln!(dot, "digraph promoted{} {{", promoted)?;
    } else {
        writeln!(dot, "digraph Mir {{")?;
    }

    // Global graph properties
    writeln!(dot, r#"    graph [fontname="monospace"];"#)?;
    writeln!(dot, r#"    node [fontname="monospace"];"#)?;
    writeln!(dot, r#"    edge [fontname="monospace"];"#)?;

    // Nodes
    for (block, _) in mir.basic_blocks().iter_enumerated() {
        write_node(block, mir, breakpoints, promoted, &mut dot)?;
    }

    // Edges
    for (source, _) in mir.basic_blocks().iter_enumerated() {
        write_edges(source, mir, &mut dot)?;
    }
    writeln!(dot, "}}")?;
    w.write_str(::std::str::from_utf8(&::cgraph::Graph::parse(dot).unwrap().render_dot().unwrap()).unwrap())
}

/// Write a graphviz HTML-styled label for the given basic block, with
/// all necessary escaping already performed. (This is suitable for
/// emitting directly, as is done in this module, or for use with the
/// `LabelText::HtmlStr` from libgraphviz.)
fn write_node_label<W: Write>(
    block: BasicBlock,
    mir: &Mir,
    breakpoints: LocalBreakpoints,
    promoted: Option<usize>,
    w: &mut W,
) -> fmt::Result {
    let data = &mir[block];

    write!(w, r#"<table border="0" cellborder="1" cellspacing="0">"#)?;

    // Basic block number at the top.
    write!(w, r#"<tr><td {attrs}>{blk}</td></tr>"#,
           attrs=r#"bgcolor="gray" align="center""#,
           blk=node(promoted, block))?;

    // List of statements in the middle.
    if !data.statements.is_empty() {
        write!(w, r#"<tr><td align="left" balign="left">"#)?;
        for (stmt_index, statement) in data.statements.iter().enumerate() {
            if breakpoints.breakpoint_exists(block, stmt_index) {
                write!(w, "+ ")?;
            } else {
                write!(w, "&nbsp; ")?;
            }
            if ::should_hide_stmt(statement) {
                write!(w, "&lt;+&gt;<br/>")?;
            } else {
                write!(w, "{}<br/>", escape(statement))?;
            }
        }
        write!(w, "</td></tr>")?;
    }

    // Terminator head at the bottom, not including the list of successor blocks. Those will be
    // displayed as labels on the edges between blocks.
    let mut terminator_head = String::new();
    data.terminator().kind.fmt_head(&mut terminator_head).unwrap();
    write!(w, r#"<tr><td align="left">{}</td></tr>"#, dot::escape_html(&terminator_head))?;

    // Close the table
    writeln!(w, "</table>")
}

/// Write a graphviz DOT node for the given basic block.
fn write_node<W: Write>(block: BasicBlock, mir: &Mir, breakpoints: LocalBreakpoints, promoted: Option<usize>, w: &mut W) -> fmt::Result {
    // Start a new node with the label to follow, in one of DOT's pseudo-HTML tables.
    write!(w, r#"    "{}" [shape="none", label=<"#, node(promoted, block))?;
    write_node_label(block, mir, breakpoints, promoted, w)?;
    // Close the node label and the node itself.
    writeln!(w, ">];")
}

/// Write graphviz DOT edges with labels between the given basic block and all of its successors.
fn write_edges<W: Write>(source: BasicBlock, mir: &Mir, w: &mut W) -> fmt::Result {
    let terminator = mir[source].terminator();
    let labels = terminator.kind.fmt_successor_labels();

    for (&target, label) in terminator.successors().zip(labels) {
        writeln!(w, r#"    {} -> {} [label="{}"];"#, node(None, source), node(None, target), label)?;
    }

    Ok(())
}

fn node(promoted: Option<usize>, block: BasicBlock) -> String {
    if let Some(promoted) = promoted {
        format!("promoted{}.{}", promoted, block.index())
    } else {
        format!("bb{}", block.index())
    }
}

fn escape<T: Debug>(t: &T) -> String {
    dot::escape_html(&format!("{:?}", t))
}
