## now
"communication" between cmd and expr mode
lifetime cleanup/rethink

## future
decent error handling
does "COMPILE" make sense?
  dry run?
type inference (?)
all types
apply that parsing/inference to command output!?
loops
functions
scopes
literals for s[] l[] d[]
set notation (s[x | x in ys, x > 2])
pipes/streams

## cleanup
proper support for associativity, not just "bin" in binop
lifetime of AST/Env contents. Ideally, they would be references to the env? How to build up original values which are made deep nested? Box or Rc?
presumably no need to move the Expr into the eval fn either?!?
