## now
basic type system to make eval possible
proper types for repl (particularly eval)

## future
decent error handling
does "COMPILE" make sense?
  dry run?
type inference (?)
all types
loops
functions
scopes
literals for s[] l[] d[]
set notation (s[x | x in ys, x > 2])
pipes/streams

## cleanup
organize into lib/bin
proper support for associativity, not just "bin" in binop
lifetime of AST/Env contents. Ideally, they would be references to the env? How to build up original values which are made deep nested? Box or Rc?
