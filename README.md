# hard shell

## requirements
* shell environment primary
* easy to understand/view environment. a la Hoare logic: id|T: val
* PATH concept is part of that environment
* `foo` executes the program identified by foo, if it is part of PATH
* can compile the scripts you save
* value semantics for stuff like ifs/folds/etc
* has to have pipes, streams, etc
* stringly type inference
* write wrappers for all cool modern syscalls, bpf, etc..
* ergonomics above all
* soundness above all else
* performance above all else

## basics
### types
* exe
* file
* i/u8,16,32,64,128,256 (?)
* string
* path
* ip4/6
* stream
* "columns" (tuple?)
* vector
* dict
* socket
* function (??)

### expressions (operations?)
value
f expr
expr binop expr
unop expr
if expr then expr else expr
let v = expr
for v in expr do expr

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

1 is boring and doesn't make any interesting "insight"
2 seems the best.
3 requires some insanity with inotify, but is the most nerdy.

## implicit command bindings for unbound stuff
### pros
if looking up the iden 'ls' gets you a function object, it neatly makes them the same.
### cons
can bind over important stuff, but OTOH, don't do it. can forbid some blacklist or just anything in $PATH (might make binding slow)
### notes
Have to have special identifier syntax for files which can be anything?
e.g. let 33 = 32 is not valid, but if you put a file called 33 in /usr/bin, 33 should run it...jo

## Is value semantics incompatible with command semantics
Probably..

e.g.
zsh> x=35
zsh> $x
command not found: 35

vs.
harsh> let x = 35
harsh> x
35: u64

shell's default stance: everything is a command
want: REPL where commands are highly favored, but you can just type 3+4 and get 7

## New Direction
two modes:
cmd: like a shell, with a clunky repl
expr: like a repl, with a clunky shell

cmd> ls
foo bar
cmd> :m
expr> let x = foo
42: int
expr> :m
cmd> ls $(x)
foo
cmd> expr
expr> x
foo
expr> $(ls -l x)
rw.r..r.. 4096 bb bb foo


