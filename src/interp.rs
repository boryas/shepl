use std::io::Write;
use std::collections::HashMap;

use crate::Err;
use crate::ast::{BinOp, Expr};
use crate::parse::expr;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    UInt64(u64),
    Str(String),
}

#[derive(Debug, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Value>
}

impl Env {
    pub fn new() -> Env {
        Env { bindings: HashMap::new() }
    }
}

fn eval_iden(env: &Env, name: &str) -> Result<Value, Err> {
    match env.bindings.get(name) {
        Some(v) => Ok(v.clone()),
        _ => {
            println!("no binding for {}: {:?}", name, env);
            Err(Err::Env)
        }
    }
}

fn eval_apply(env: &mut Env, name: &str, args: Vec<Expr>) -> Result<Value, Err> {
    let mut script = std::process::Command::new(name);
    for arg in args {
        let _ = match eval(env, arg)? {
            Value::UInt64(u) => script.arg(u.to_string()),
            Value::Str(a) => script.arg(a),
        };
    }
    let output = script.output().expect("failed to execute cmd");
    let s = String::from_utf8(output.stdout).expect("invalid UTF-8 output");
    Ok(Value::Str(s.to_string()))
}

fn eval_binop(env: &mut Env, op: BinOp, left: Box<Expr>, right: Box<Expr>) -> Result<Value, Err> {
    let lv = eval(env, *left)?;
    let rv = eval(env, *right)?;
    match (lv, rv) {
        (Value::UInt64(l), Value::UInt64(r)) =>
            match op {
                BinOp::Add => Ok(Value::UInt64(l+r)),
                BinOp::Sub => Ok(Value::UInt64(l-r)),
                BinOp::Mul => Ok(Value::UInt64(l*r)),
                BinOp::Div => Ok(Value::UInt64(l/r)),
            },
        _ => Err(Err::Eval)
    }
}

fn eval_cond(env: &mut Env, pred: Box<Expr>, br1: Box<Expr>, br2: Box<Expr>) -> Result<Value, Err> {
    let p = eval(env, *pred)?;
    let truth =
        match p {
            Value::UInt64(u) => u > 0,
            Value::Str(s) => !s.is_empty()
        };
    if truth {
        eval(env, *br1)
    } else {
        eval(env, *br2)
    }
}

fn eval_assign(env: &mut Env, name: &str, expr: Box<Expr>) -> Result<Value, Err> {
    let v = eval(env, *expr)?;
    env.bindings.insert(name.to_string(), v.clone());
    Ok(v.clone())
}

fn eval(env: &mut Env, expr: Expr) -> Result<Value, Err> {
    match expr {
        Expr::UInt64(i) => Ok(Value::UInt64(i)),
        Expr::Str(s) => Ok(Value::Str(s)),
        Expr::Iden(n) => eval_iden(env, &n),
        Expr::Apply(f, args) => eval_apply(env, &f, args),
        Expr::BinOp(op, e1, e2) => eval_binop(env, op, e1, e2),
        Expr::Cond(pred, br1, br2) => eval_cond(env, pred, br1, br2),
        Expr::Assign(n, expr) => eval_assign(env, &n, expr),
    }
}

fn parse(input: &str) -> Result<Expr, Err> {
    match expr(input) {
        Ok(("", e)) => Ok(e),
        _ => Err(Err::Parse),
    }
}

fn read() -> Result<String, Err> {
    let mut l = String::new();
    match std::io::stdin().read_line(&mut l) {
        Ok(_) => Ok(l),
        _ => Err(Err::Read),
    }
}

fn read_eval(env: &mut Env) -> Result<String, Err> {
    let line = read()?;
    let expr = parse(&line)?;
    match eval(env, expr)? {
        Value::UInt64(u) => Ok(format!("{}: u64", u)),
        Value::Str(s) => Ok(format!("{}: str", s))
    }
}

pub fn repl() {
    let mut env = Env::new();
    loop {
        print!("$ ");
        std::io::stdout().flush().expect("failed to flush prompt");
        match read_eval(&mut env) {
            Ok(s) => println!("{}", s),
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
}
