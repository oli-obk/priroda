#!/bin/sh

export PATH="$HOME/.cargo/bin:$PATH"
export ROCKET_ENV=production
export ROCKET_PORT=$PORT
rustup run nightly target/release/priroda example.rs --sysroot $(rustc +nightly --print sysroot)
