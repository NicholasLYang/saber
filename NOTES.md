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
