# Saber

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md) 

Saber is a row polymorphic language that focuses on first class
functions, a lightweight syntax and fast, efficient code. However,
before any of those goals, I just need to build the basic
compiler. Right now I'm just compiling it to WebAssembly, but
hopefully I'll be able to port it to other platforms.

## TODO
- General
  - ~~Fix all the damn copies and bad, non idiomatic Rust.~~
  - ~~Probably add a ref count for types (really no need to copy them
    since they're relatively immutable)~~
- Parsing
  - ~~Convert to expression based syntax~~
  - Trailing comma?
- Typechecking
  - ~~Typecheck functions~~
  - ~~Typecheck records~~
  - ~~Add named types~~
  - Constraint solve type variables
  - Add sum and product types
- Lexical Analysis
  - Get closures working
- Code gen
  - ~~Get something working.~~
  - ~~Add local variables~~
  - ~~Add control flow~~ (Kinda)
  - ~~Add function calls~~
  - ~~Add nested functions~~ 
  - ~~Add first class functions~~
  - ~~Add structs/records~~
  - ~~Add strings~~
  - Garbage collection!
- Tests/Debugging
  - ~~Write some tests~~
  - ~~Add locations in errors with row/col~~
  - ~~Add better error messages using name and type tables~~

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

## WASM Notes

Locals are stored in local entries in the code section, and params are
stored in the function types section, but they share the same index
space.

Remember what is an actual parameter for an op (i.e. goes right after
the opcode) and what is implicitly read from the stack.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](code_of_conduct.md). 
By participating in this project you agree to abide by its terms.

We highly recommend reading it over carefully before contributing.