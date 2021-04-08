use crate::Mode;

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Arg {
    Raw(String),
    Rec(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct Cmd {
    pub cmd: String,
    pub args: Vec<Arg>,
}

#[derive(Debug, PartialEq)]
pub enum Special {
    Help,
    Quit,
    Mode(Option<Mode>),
}

#[derive(Debug, PartialEq)]
pub enum Single {
    Integer(i128),
    Str(String),
    Iden(String),
    Paren(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Single(Single),
    BinOp(BinOp, Box<Expr>, Box<Expr>),    // expr + expr
    Cond(Box<Expr>, Box<Expr>, Box<Expr>), // if expr then expr else expr
    Assign(String, Box<Expr>),             // let iden = expr
    Cmd(Cmd),                              // iden arg1 arg2 ... argN
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Cmd(Cmd),
    Special(Special),
}

#[derive(Debug)]
pub enum Input {
    Expr(Expr),
    Cmd(Cmd),
    Special(Special),
}
