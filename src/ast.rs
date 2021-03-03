#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub struct Cmd {
    cmd: String,
    args: Vec<Expr>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    UInt64(u64),
    Str(String),
    Iden(String),
    Cmd(String, Vec<Expr>), // iden arg1 arg2 ... argN
    BinOp(BinOp, Box<Expr>, Box<Expr>), // expr + expr
    Cond(Box<Expr>, Box<Expr>, Box<Expr>), // if expr then expr else expr
    Assign(String, Box<Expr>), // let iden = expr
}
