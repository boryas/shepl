use std::collections::HashMap;
use std::fmt;
use std::io::Write;

use crate::ast::{Arg, BinOp, Cmd, Expr, Mode, Single, Special, Stmt};
use crate::parse::stmt;
use crate::Err;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Integer(i128),
    Str(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            bindings: HashMap::new(),
        }
    }
}

fn eval_iden(env: &Env, i: &str) -> Result<Value, Err> {
    match env.bindings.get(i) {
        Some(v) => Ok(v.clone()),
        _ => {
            println!("no binding for {}: {:?}", i, env);
            Err(Err::Env)
        }
    }
}

fn eval_single(env: &mut Env, e: Single) -> Result<Value, Err> {
    match e {
        Single::Integer(i) => Ok(Value::Integer(i)),
        Single::Str(s) => Ok(Value::Str(s)),
        Single::Iden(i) => eval_iden(env, &i),
        Single::Paren(e) => eval(env, *e),
    }
}

fn eval_cmd(env: &mut Env, cmd: Cmd) -> Result<Value, Err> {
    Ok(Value::Str(run_cmd(env, cmd)?))
}

fn eval_binop(env: &mut Env, op: BinOp, left: Box<Expr>, right: Box<Expr>) -> Result<Value, Err> {
    let lv = eval(env, *left)?;
    let rv = eval(env, *right)?;
    match (lv, rv) {
        (Value::Integer(l), Value::Integer(r)) => match op {
            BinOp::Add => Ok(Value::Integer(l + r)),
            BinOp::Sub => Ok(Value::Integer(l - r)),
            BinOp::Mul => Ok(Value::Integer(l * r)),
            BinOp::Div => Ok(Value::Integer(l / r)),
        },
        _ => Err(Err::Eval),
    }
}

fn eval_cond(env: &mut Env, pred: Box<Expr>, br1: Box<Expr>, br2: Box<Expr>) -> Result<Value, Err> {
    let p = eval(env, *pred)?;
    let truth = match p {
        Value::Integer(i) => i != 0,
        Value::Str(s) => !s.is_empty(),
    };
    if truth {
        eval(env, *br1)
    } else {
        eval(env, *br2)
    }
}

fn eval_assign(env: &mut Env, name: &str, expr: Box<Expr>) -> Result<Value, Err> {
    let v = eval(env, *expr)?;
    env.bindings.insert(name.to_string(), v);
    eval_iden(env, name)
}

fn eval(env: &mut Env, expr: Expr) -> Result<Value, Err> {
    match expr {
        Expr::Single(e) => eval_single(env, e),
        Expr::Cmd(cmd) => eval_cmd(env, cmd),
        Expr::BinOp(op, e1, e2) => eval_binop(env, op, e1, e2),
        Expr::Cond(pred, br1, br2) => eval_cond(env, pred, br1, br2),
        Expr::Assign(n, expr) => eval_assign(env, &n, expr),
    }
}

fn parse(input: &str, mode: &Mode) -> Result<Stmt, Err> {
    match stmt(input, mode) {
        Ok(("", s)) => Ok(s),
        _ => Err(Err::Parse),
    }
}

fn run_cmd(env: &mut Env, cmd: Cmd) -> Result<String, Err> {
    let mut proc = std::process::Command::new(cmd.cmd);
    for arg in cmd.args {
        let a = match arg {
            Arg::Raw(a) => a,
            Arg::Rec(e) => format!("{}", eval(env, *e)?),
        };
        proc.arg(a);
    }
    let output = proc.output().expect("failed to execute cmd");
    Ok(String::from_utf8(output.stdout).expect("invalid UTF-8 output"))
}

fn read() -> Result<String, Err> {
    let mut l = String::new();
    match std::io::stdin().read_line(&mut l) {
        Ok(_) => Ok(l),
        _ => Err(Err::Read),
    }
}

fn toggle_mode(mode: &Mode, new_mode: Option<Mode>) -> Mode {
    match new_mode {
        Some(m) => m,
        None => match mode {
            Mode::Cmd => Mode::Expr,
            Mode::Expr => Mode::Cmd,
        },
    }
}

fn read_eval(env: &mut Env, mode: &mut Mode) -> Result<String, Err> {
    let line = read()?;
    let stmt = parse(&line, mode)?;
    match stmt {
        Stmt::Expr(e) => Ok(format!("{}", eval(env, e)?)),
        Stmt::Cmd(c) => run_cmd(env, c),
        Stmt::Special(s) => match s {
            Special::Help => Ok(format!("mode: {:?} cmd:expr::shell:repl", *mode)),
            Special::Quit => Err(Err::Eval),
            Special::Mode(o) => {
                *mode = toggle_mode(mode, o);
                Ok(format!(""))
            }
        },
    }
    /*
     */
}

pub fn repl() {
    let mut env = Env::new();
    let mut mode = Mode::Cmd;
    loop {
        print!("{:?}> ", mode);
        std::io::stdout().flush().expect("failed to flush prompt");
        match read_eval(&mut env, &mut mode) {
            Ok(s) => println!("{}", s),
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
}
