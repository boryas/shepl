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
expr expr
expr binop expr
unop expr
if expr then expr else expr
for v in expr do expr
