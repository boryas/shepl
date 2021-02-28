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
get rid of unneeded lets/Oks when you can just return combinator calls
proper support for associativity, not just "bin" in binop
think of lifetimes cleverly?

store input lines
everything thence refers back to it???

OR

keep copying as we do now
use clone to keep it explicit
