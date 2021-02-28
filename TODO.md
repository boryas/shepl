## now
basic type system to make eval possible
define a simple env
interpreter
hack on path/env till you can run ls
proper types for repl (particularly eval)

## future
all types
loops
functions
pipes/streams
type inference
decent error handling

## cleanup
organize into lib/main
play with no-copy on &str instead of String (input lifetime)
proper support for associativity, not just "bin" in binop
lifetime of AST/Env contents

## important Qs
integer types?!
strs vs idens
applies vs idens

What (if anything?) is the difference between

$ let ls = 3
$ ls
> 3

and

$ ls
> foo bar baz

what about

$ let x = 3
$ ls x
should it be `ls 3` or `ls x` ??? How does the let affect it?

trivial solution: $x for bound vars
solution 2:       function types and proper apply, unbound functions are a
                  lookup on PATH for a '-> Str'
solution 3:       pre-bind PATH, use when parsing/eval-ing
