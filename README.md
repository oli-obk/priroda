# Priroda

Priroda is a graphical (UI in browser) debugger for Rust programs

## Setup

You need a few things before you can get started. At the very minimum you need to have
the graphviz libraries present.

* debian/ubuntu: `apt install libgraphviz-dev`

Next, you're going to want a libstd with full MIR. The easiest way to obtain this is via
`cargo miri`:

```bash
# Install cargo miri:
rustup component add miri
# Compile libstd:
cargo miri setup
# Set the MIRI_SYSROOT environment variable to the path printed by the setup command:
export MIRI_SYSROOT=...
```

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
ideas or help with running or hacking on Priroda, you can ask at [`#miri`] on the
rust-lang zulip.

[`#miri`]: https://rust-lang.zulipchat.com/#narrow/stream/269128-miri

### Miri

This project depends entirely on [Miri](https://github.com/rust-lang/miri).
So if you want to improve something that we fail to interpret, add a unit test
to Miri and fix it there.

## License

Licensed under either of

* Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
* MIT license ([LICENSE-MIT](LICENSE-MIT))

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as above, without any
additional terms or conditions.
