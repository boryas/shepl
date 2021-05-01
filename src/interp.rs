use std::collections::HashMap;
use std::fmt;
use std::io::Write;

use crate::{
    ast::{Arg, BinOp, Cmd, Expr, Single, Special, Stmt},
    err,
    lex::{lex, Op, Lexemes},
    parse::parse,
    Err, Mode,
};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, sequence::delimited,
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Integer(i128),
    Str(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
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
        Single::Paren(e) => eval_expr(env, *e),
    }
}

fn eval_binop(env: &mut Env, op: Op, left: Box<Expr>, right: Box<Expr>) -> Result<Value, Err> {
    let lv = eval_expr(env, *left)?;
    let rv = eval_expr(env, *right)?;
    match (lv, rv) {
        (Value::Integer(l), Value::Integer(r)) => match op {
            Op::Add => Ok(Value::Integer(l + r)),
            Op::Sub => Ok(Value::Integer(l - r)),
            Op::Mul => Ok(Value::Integer(l * r)),
            Op::Div => Ok(Value::Integer(l / r)),
            Op::Xor => Ok(Value::Integer(l ^ r)),
            Op::BitAnd => Ok(Value::Integer(l & r)),
            Op::BitOr => Ok(Value::Integer(l | r)),
            Op::Eq => Ok(Value::Boolean(l == r)),
            Op::Neq => Ok(Value::Boolean(l != r)),
            Op::Gt => Ok(Value::Boolean(l > r)),
            Op::Gte => Ok(Value::Boolean(l >= r)),
            Op::Lt => Ok(Value::Boolean(l < r)),
            Op::Lte => Ok(Value::Boolean(l <= r)),
            _ => Err(Err::Eval),
        },
        (Value::Boolean(l), Value::Boolean(r)) => match op {
            Op::Eq => Ok(Value::Boolean(l == r)),
            Op::Neq => Ok(Value::Boolean(l != r)),
            Op::And => Ok(Value::Boolean(l && r)),
            Op::Or => Ok(Value::Boolean(l || r)),
            _ => Err(Err::Eval),
        },
        _ => Err(Err::Eval),
    }
}

fn eval_cond(env: &mut Env, pred: Box<Expr>, br1: Box<Expr>, br2: Box<Expr>) -> Result<Value, Err> {
    let p = eval_expr(env, *pred)?;
    let truth = match p {
        Value::Boolean(b) => b,
        Value::Integer(i) => i != 0,
        Value::Str(s) => !s.is_empty(),
    };
    if truth {
        eval_expr(env, *br1)
    } else {
        eval_expr(env, *br2)
    }
}

fn eval_assign(env: &mut Env, name: &str, expr: Box<Expr>) -> Result<Value, Err> {
    let v = eval_expr(env, *expr)?;
    env.bindings.insert(name.to_string(), v);
    eval_iden(env, name)
}

fn eval_expr(env: &mut Env, expr: Expr) -> Result<Value, Err> {
    match expr {
        Expr::Single(e) => eval_single(env, e),
        Expr::Cmd(cmd) => run_cmd(env, cmd),
        Expr::BinOp(op, e1, e2) => eval_binop(env, op, e1, e2),
        Expr::Cond(pred, br1, br2) => eval_cond(env, pred, br1, br2),
        Expr::Assign(n, expr) => eval_assign(env, &n, expr),
    }
}

fn run_cmd(env: &mut Env, cmd: Cmd) -> Result<Value, Err> {
    let mut proc = std::process::Command::new(cmd.cmd);
    for arg in cmd.args {
        let a = match arg {
            Arg::Raw(a) => a,
            Arg::Rec(e) => format!("{}", eval_expr(env, *e)?),
        };
        proc.arg(a);
    }
    match proc.output() {
        Ok(o) => Ok(Value::Str(
            String::from_utf8(o.stdout).expect("invalid UTF-8 output"),
        )),
        Err(e) => {
            println!("cmd failed: {:?}", e);
            Err(Err::Run)
        }
    }
}

fn read() -> Result<(usize, String), Err> {
    let mut l = String::new();
    match std::io::stdin().read_line(&mut l) {
        Ok(sz) => Ok((sz, l)),
        _ => Err(Err::Read),
    }
}

fn do_toggle_mode(mode: &Mode, new_mode: Option<Mode>) -> Mode {
    match new_mode {
        Some(m) => m,
        None => match mode {
            Mode::Shell => Mode::Repl,
            Mode::Repl => Mode::Shell,
        },
    }
}

fn eval(env: &mut Env, stmt: Stmt) -> Result<Value, Err> {
    match stmt {
        Stmt::Expr(e) => eval_expr(env, e),
        Stmt::Cmd(c) => run_cmd(env, c),
    }
}

fn help(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, _) = alt((tag("!?"), tag("!help"), tag("!h")))(input)?;
    Ok((input, Special::Help))
}

fn quit(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, _) = alt((tag("!quit"), tag("!q")))(input)?;
    Ok((input, Special::Quit))
}

fn toggle_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = alt((tag("!mode"), tag("!m")))(input)?;
    Ok((input, None))
}

fn shell_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = tag("!shell")(input)?;
    Ok((input, Some(Mode::Shell)))
}

fn repl_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = tag("!repl")(input)?;
    Ok((input, Some(Mode::Repl)))
}

fn mode(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, m) = alt((toggle_mode, shell_mode, repl_mode))(input)?;
    Ok((input, Special::Mode(m)))
}

fn special(input: &str) -> Option<Special> {
    match delimited(multispace0, alt((help, mode, quit)), multispace0)(input) {
        Ok(("", s)) => Some(s),
        _ => None,
    }
}

pub fn replnsh() {
    let mut env = Env::new();
    let mut mode = Mode::Shell;
    loop {
        print!("{:?}> ", mode);
        std::io::stdout().flush().expect("failed to flush prompt");
        let line = match read() {
            Ok((0, _)) => break, // eof, clever that it knows I don't use `line`..
            Ok((_, l)) => l,
            _ => panic!("input error!"),
        };
        if let Some(s) = special(&line) {
            match s {
                Special::Help => println!("mode: {:?} cmd:expr::shell:repl", mode),
                Special::Quit => break,
                Special::Mode(o) => {
                    mode = do_toggle_mode(&mode, o);
                }
            };
            continue;
        };
        let lx = match lex(&line, &mut mode) {
            Ok(("", lx)) => lx,
            Ok((rest, _)) => {
                println!("didn't lex: {}", rest);
                continue;
            }
            Err(e) => {
                println!("lex error: {:?}", e);
                continue;
            }
        };
        let lxs = Lexemes::new(&lx[..]);
        match parse(lxs, &mut mode) {
            Ok((_, s)) => match eval(&mut env, s) {
                Ok(v) => println!("{}", v),
                Err(e) => {
                    println!("{:?}", e);
                }
            },
            Err(e) => {
                println!("{:?}", e);
            }
        }
    }
}
