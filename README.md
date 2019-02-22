# Saber

Saber is a row polymorphic language that focuses on first class
functions, a lightweight syntax and fast, efficient code. However,
before any of those goals, I just need to build the basic
compiler. Right now I'm just compiling it to WebAssembly, but
hopefully I'll be able to port it to other platforms.

## TODO
- General
  - Fix all the damn copies and bad, non idiomatic Rust.
  - Probably add a ref count for types (really no need to copy them
    since they're relatively immutable)
- Typechecking
  - Typecheck functions
  - Typecheck records
  - Add named types
- Code gen
  - Get something working.
  - Figure out a better way than string literals.
  - Probably build something like TS' emitter.
