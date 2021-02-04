# Saber

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md) 

Saber is a row polymorphic language that focuses on first class
functions, a lightweight syntax and fast, efficient code. However,
before any of those goals, I just need to build the basic
compiler. Right now I'm just compiling it to WebAssembly, but
hopefully I'll be able to port it to other platforms.

## Contributing

Contributors are welcome! To develop on Saber, all you need is Rust, either stable 
or nightly. Saber compiles on rustc 1.47.0 stable and rustc 1.46.0-nightly.

I'm currently running Saber on macOS Catalina, but I don't see why it shouldn't work 
on Linux or Windows 

### Setup

The entire saber codebase builds into a single executable containing the runtime, 
compiler and CLI. This isn't ideal, but it's what works.

Run `cargo build` to get the executable or `cargo run` to run the executable. We use clap so there's some help text specific subcommands

If you're using `cargo run`, add the subcommands after `--`:
```
cargo run -- run tests/all/factorial.sbr
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](code_of_conduct.md). 
By participating in this project you agree to abide by its terms.

We highly recommend reading it over carefully before contributing.

## Design

Goals:

- Relatively good performance via browser JIT.
  - Avoid common performance gotchas like overly dynamic dispatch,
    reflection, etc.
- Good (type!) compatibility with existing JS
  - Allow for interfacing with completely untyped JS with potential
    runtime checks (performance penalty?)
- Lightweight syntax. Avoid using keywords when possible (however,
  will affect syntax highlighting)
