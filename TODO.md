# TODO
- General
  - ~~Fix all the damn copies and bad, non idiomatic Rust.~~
  - ~~Probably add a ref count for types (really no need to copy them
    since they're relatively immutable)~~
- Parsing
  - ~~Convert to expression based syntax~~
  - Trailing comma?
  - Figure out injective parsing
- Typechecking
  - ~~Typecheck functions~~
  - ~~Typecheck records~~
  - ~~Add named types~~
  - Constraint solve type variables (kinda done?)
  - Add sum and product types
  - Add generics
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
  - ~~Garbage collection!~~
  - Hook up garbage collection to code gen
  - Figure out an SSR IR
- Tests/Debugging
  - ~~Write some tests~~
  - ~~Add locations in errors with row/col~~
  - ~~Add better error messages using name and type tables~~
  - Get automated testing
- Build System
  - Automate TypeScript compilation for runtime
- Tooling
  - Get emacs plugin
  - Start figuring out LSP
- Symbol Table
  - Clean up, oh man it's messy rn
  - Consolidate var stuff into Entry and make an Option<FunctionInfo>
  - Make it easier to get a function's captures. Consolidate function scope with function entry?