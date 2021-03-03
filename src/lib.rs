pub mod ast;
pub mod interp;
pub mod parse;

#[derive(Debug, PartialEq)]
pub enum Err {
    Env,
    Eval,
    Parse,
    Read,
}
