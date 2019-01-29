# Priroda

Remote sensing bugs from your comfy ~~zero~~ gravity hack center in ~~orbit~~
the browser of your choice.

## Setup

You need a few things before you can get started. At the very minimum you need to have
the graphviz libraries present.

* debian/ubuntu: `apt install libgraphviz-dev`

Next, you're going to want a libstd with full MIR.The easiest way to obtain this is via
`cargo miri` (install with `cargo +nightly install --force --git https://github.com/solson/miri miri`).

## Features

* Supports commands known from gdb
  * next, step, continue
* Inspect memory of all stack frames visually
* Follow pointers by clicking hyperlinks
* Track your progress through a function in a graph of the MIR
* Style your debugging experience with CSS

## Usage

`cargo run some_rust_file.rs` will automatically start a http server and open a
browser. UI is changing rapidly right now, so you need to figure out how to use
it by yourself (or by asking on irc) for now.

## Contributing and getting help

Check out the issues on this GitHub repository for some ideas. There's lots that
needs to be done that I haven't documented in the issues yet, however. For more
ideas or help with running or hacking on Priroda, you can contact me (`oli_obk`)
on Mozilla IRC in any of the Rust IRC channels (`#rust`, `#rust-offtopic`, etc).

### Miri

This project depends entirely on [Miri](https://github.com/solson/miri).
So if you want to improve something that we fail to interpret, add a unit test
to Miri and fix it there.

## License

Licensed under either of

* Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE)
* MIT license ([LICENSE-MIT](LICENSE-MIT)

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as above, without any
additional terms or conditions.
