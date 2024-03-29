

## WASM Notes

Locals are stored in local entries in the code section, and params are
stored in the function types section, but they share the same index
space.

Remember what is an actual parameter for an op (i.e. goes right after
the opcode) and what is implicitly read from the stack.

## Symbol Table

I need a way to map names to indices for functions and
variables. These names depend on scope, so the natural inclination is
to use a symbol table. However I'm trying to figure out if indices
should be generated in the typechecker (prevents having to make
indices optional, but kinda breaks modularity) or generated in the
code generator (a little messier with types but more self contained)

Update: I went with indices generated in the typechecker. Since the
semantic analysis already was doing a lot of the work, I figured
why not. 

## References
https://en.wikipedia.org/wiki/Lambda_lifting
https://boats.gitlab.io/blog/post/the-problem-of-effects/
https://gafter.blogspot.com/2006/08/tennents-correspondence-principle-and.html

## Feature Ideas

`this` or `self` keyword in anonymous functions for easier recursion:
```
\(n) => {
  if n < 0 {
    return 1;
  } else {
    return n * this(n - 1)
  }
}
```

Typechecking based on typeclass/trait constraints, then with interop, turn these constraint checks
into runtime ones.

## WASM WTFs
The types section determines the function index space, NOT the functions sections. The
more accurate name for the functions section is the *local* functions section and the
more accurate name for the types section is the declarations section (a la C headers).

## Call Indirect
I need the expected function signature as an index. Which means I should probably collect the function signatures
in a HashSet/HashMap then when I come across a call expression, figure out the expected type of the function, 
look up in said map and then insert the id.

Update: I originally started with a HashMap, then I figured that a HashMap was an overoptimization and
went with just an array. Honestly once the language starts being used by someone semi-seriously, I'll optimize
that.

Update 2: I now split Call expressions into ExprT::CallDirect and ExprT::CallIndirect. This makes codegen
a lot easier for calls.

## Closures

Should closures copy or reference variables? If they're copied that makes my life a lot easier. But it's probably
super inefficient, so I'd probably need to make it copy-on-write, but that'd also be hard. 

## Errors
Use a crate? https://github.com/brendanzab/codespan

Update: Started using it

## Memory Allocation

Global #0 is gonna be pointer to current heap max

## Setup Captures

Problem: function bindings, i.e. StmtT::Function, are not actually variables and therefore cannot store data.
Solution: Make them actual variables and have them store data.

## Build System
Too many things to do right now. Should have one simple compile script that runs

## Make Saber an OCaml with good tooling
- Query based compiler
  - Have editing be mutation queries
  - After mutation query, update watching queries
- Language server
- Packages (ooh interop with Rust?)
  - Can send values to Saber by wrapping in `Rc` (or `Arc`?)
  ```rust
    use saber::foo;
    let v = vec![10, 12, 34];
    foo(Rc::new(v));
  ```
  - Does it need to be `Pin`?
  - Or we use Rust's move semantics. Ooh have a SaberSerialize trait. Or use serde?
  - Have a serde target that is just Saber's underlying memory model. `to_writer`

## Closure conversion

```js
let foo = () => {
  let a = 10;
  let bar = () => {
    printInt(a);
  }
}
```
```ts
let foo = (env: (ptr)) => {
  let a: int = 10;
  let bar = (env: [ptr, int]) => {
    printInt(env[1]);
  }
}
```

```js
let foo = () => {
  let a = 10;
  let bar = () => {
    let baz = () => {
        printInt(a);
    }
  }
}
```
```ts
let foo = (env: []) => {
  let a: int = 10;
  let bar = (env: [ptr, int]) => {
    let baz = (env: (ptr)) => {
      printInt(env[0][1]);      
    }
  }
}
```

Tell the function that initially captures the value to add it to its environment struct.
Then build field chain and rewrite var to be a field access


## Control Flow Analysis

For functions, replace end expression in block with a return statement

This allows us to have function bodies that are a vector of statements.

Then for control flow, check that there is a return statement at the end.

## Traits

There should be some form of traits within the language. Basically, a way to define
an abstract interface that multiple types can implement.