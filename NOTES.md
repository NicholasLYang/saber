## Symbol Table

I need a way to map names to indices for functions and
variables. These names depend on scope, so the natural inclination is
to use a symbol table. However I'm trying to figure out if indices
should be generated in the typechecker (prevents having to make
indices optional, but kinda breaks modularity) or generated in the
code generator (a little messier with types but more self contained)

## References
https://en.wikipedia.org/wiki/Lambda_lifting

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