pub mod ast;
pub mod err;
pub mod interp;
pub mod lex;
pub mod parse;

#[derive(Debug, PartialEq)]
pub enum Err {
    Env,
    Eval,
    Parse,
    Read,
    Run,
}

#[derive(Debug, PartialEq)]
pub enum Mode {
    Repl,
    Shell,
}
