# shepl

A modal shell that tries to be a REPL too.

## The Problem
bash:
```
$ ls foo
bar baz
$ d=foo
$ ls d
No such file or directory
$ ls $d
bar baz
$ 3+4
Command not found
```

Everyone is used to this, and it sure works. But a REPL is often more powerful
python:
```
>>> d = "foo"
>>> dirs = subprocess.check_output(["ls", d]).split('\n')
['bar', 'baz']
>>> for d in dirs:
...
>>> 3 + 4
7
```

But obviously, 1subprocess.check_output(["ls", d])` doesn't hold a candle to
`ls $d` in terms of shell-y expressiveness.

Is it possible to thread the needle and design a shell-repl hybrid that gets
the best of both worlds: A rich typed environment where you can interactively
program and a simple execution environmnent where you can manage running
commands? The key missing part, in my opinion, is tight integration between
the two. It is annoying to try to export the artifacts of a REPL session back
to the shell you were in.

## The First Attempt
I started by trying to make a shell that felt more powerful, or more like a
proper programming language. I am naturally not the first to attempt this.
Fish and oil are both excellent attempts at cleaning up the specific most gunky
cruft of bash: better assignment syntax, etc. Csh, perl, scsh (scheme), rash
(racket) are great instances of powerful programming/command environments. I
quickly found that I actually had nothing new to bring to the table as I tried
to implement something like:

```
$ let dir = "foo"
$ ls dir
bar baz
```

and quickly found that I couldn't make it work. In general, substituting
variables from the environment and treating arguments as strings felt
incompatible, and the dreaded $dir felt more and more inevitable. But even
worse than just admitting defeat, this meant that the project would never
be anything more than a really bad version of fish, with nothing interesting
to distinguish it.

## The Modal Shell
I was depressedly clacking around in vim looking at my sad excuse of a parser
when I realized I could try to use modes and really have it all -- a shell
AND a repl. What if there were special commands that allowed you to seamlessly
hop between being in a shell and being in a repl? In the shell, you breezily
hammer out pipelines, everything is text, commands gobble up whitespace
delimited args, use PATH, etc. But in the repl, you can have lambdas, scopes,
structs, enums, pattern matching, loops, and more, all without the insanity of
syntax that has to pretend everything is a command line. And then you take the
values you generated in the repl and bring them with you to the shell. Then you
run a command and store it in a variable, jump back to the repl, and hack it up
with powerful code.

The silly example above, re-visited:
```
cmd> ls foo
bar baz
cmd> :m
expr> 3 + 4
7
expr> let d = "foo"
expr> $(ls $(d))
bar baz
expr> :m
cmd> ls $(d)
bar baz
```

This feels promising enough to pursue.

## requirements
* cmd mode is a good shell
* expr mode is a good repl
* easy to feed values generated in the repl to commands
* easy to hack on the output of commands in the repl
* artifacts exportable for future use

## values
* ergonomics above all
* soundness above all else
* performance above all else

## future ideas
### re-parse
parse command output into classes of types, which can be fed into repl.
* tables
* rows/columns
* files
* exes
* various types of numbers (int, float, uint, etc)
* paths
* network addresses
* streams

### notebook
modules?
run ten commands, save 8 of them to a script, run it later
