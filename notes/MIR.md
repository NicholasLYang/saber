Middle Intermediate Representation

Goals:
- Make closures less miserable to emit
- Reduce overall code generation complexity
- Add potential for optimizations

Maybe SSA?

Process
- Take all functions and place them at top level with explicit environment argument
- Replace all indexing/field accesses with explicit pointer dereferences
- Split variables into: local variables and closure variables
