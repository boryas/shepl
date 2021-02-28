extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, anychar, digit1, multispace0, multispace1, one_of},
    combinator::{map_res, recognize, verify},
    multi::{many0, separated_list1},
    sequence::{delimited, tuple},
    AsChar, IResult,
};

use std::io::Write;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    UInt64(u64),
    Str(String),
}

#[derive(Debug, PartialEq)]
pub struct Env {
    bindings: HashMap<String, Value>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    UInt64(u64),
    Str(String),
    Iden(String),
    Apply(String, Vec<Expr>), // iden arg1 arg2 ... argN
    BinOp(BinOp, Box<Expr>, Box<Expr>), // expr + expr
    Cond(Box<Expr>, Box<Expr>, Box<Expr>), // if expr then expr else expr
    Assign(String, Box<Expr>), // let iden = expr
}

#[derive(Debug, PartialEq)]
pub enum Err {
    Env,
    Eval,
    Parse,
    Read,
}

fn uint64(input: &str) -> IResult<&str, Expr> {
    let (input, u) = map_res(digit1, |ds: &str| u64::from_str_radix(&ds, 10))(input)?;
    Ok((input, Expr::UInt64(u)))
}

fn str(input: &str) -> IResult<&str, Expr> {
    // TODO: decide what characters you want in string literals
    // TODO: escape codes
    let (input, s) = delimited(tag("\""), alphanumeric0, tag("\""))(input)?;
    Ok((input, Expr::Str(s.to_string())))
}

fn iden_char(c: &char) -> bool {
    (*c).is_alphanum() || *c == '_'
}

fn valid_iden(s: &str) -> bool {
    // TODO: use a set
    !(s == "if" || s == "then" || s == "else" || s == "let")
}

fn iden(input: &str) -> IResult<&str, Expr> {
    let (input, id) = verify(
        recognize(tuple((
            verify(anychar, |c: &char| (*c).is_alpha() || *c == '_'),
            many0(verify(anychar, iden_char)),
        ))),
        valid_iden,
    )(input)?;
    Ok((input, Expr::Iden(id.to_string())))
}

fn paren(input: &str) -> IResult<&str, Expr> {
    let (input, inner) = delimited(tag("("), expr, tag(")"))(input)?;
    Ok((input, inner))
}

fn single(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((uint64, iden, str, paren))(input)?;
    Ok((input, e))
}

fn binop(input: &str) -> IResult<&str, Expr> {
    let (input, (e1, _, op, _, e2)) =
        tuple((single, multispace0, one_of("+-*/"), multispace0, single))(input)?;
    Ok((
        input,
        Expr::BinOp(
            match op {
                '+' => BinOp::Add,
                '-' => BinOp::Sub,
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                _ => unreachable!(),
            },
            Box::new(e1),
            Box::new(e2),
        ),
    ))
}

fn apply(input: &str) -> IResult<&str, Expr> {
    let (input, (f, _, v)) =
        tuple((iden, multispace1, separated_list1(multispace1, single)))(input)?;
    let name = match f {
        Expr::Iden(name) => name,
        _ => unreachable!(),
    };
    Ok((input, Expr::Apply(name, v)))
}

fn cond(input: &str) -> IResult<&str, Expr> {
    let (input, _if) = tag("if")(input)?;
    let (input, pred) = expr(input)?;
    let (input, _then) = tag("then")(input)?;
    let (input, br1) = expr(input)?;
    let (input, _else) = tag("else")(input)?;
    let (input, br2) = expr(input)?;
    Ok((
        input,
        Expr::Cond(Box::new(pred), Box::new(br1), Box::new(br2)),
    ))
}

fn assign(input: &str) -> IResult<&str, Expr> {
    let (input, _let) = tag("let")(input)?;
    let (input, iden) = delimited(multispace1, iden, multispace1)(input)?;
    let name = match iden {
        Expr::Iden(name) => name,
        _ => unreachable!(),
    };
    let (input, _eq) = tag("=")(input)?;
    let (input, expr) = expr(input)?;
    Ok((input, Expr::Assign(name, Box::new(expr))))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    delimited(multispace0, alt((assign, cond, apply, binop, single)), multispace0)(input)
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
    println!("EVAL! {:?}", expr);
    match expr {
        Expr::UInt64(i) => Ok(Value::UInt64(i)),
        Expr::Str(s) => Ok(Value::Str(s)),
        Expr::Iden(n) => eval_iden(env, &n),
        Expr::Apply(_f, _args) => Err(Err::Eval),
        Expr::BinOp(_op, _e1, _e2) => Err(Err::Eval),
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

fn repl(env: &mut Env) {
    loop {
        print!("$ ");
        std::io::stdout().flush();
        match read_eval(env) {
            Ok(s) => println!("{}", s),
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
}

fn main() {
    let mut env = Env { bindings: HashMap::new() };
    repl(&mut env);
}

#[test]
fn parse_iden() {
    assert_eq!(iden("foo"), Ok(("", Expr::Iden("foo".to_string()))));
    assert_eq!(iden("f"), Ok(("", Expr::Iden("f".to_string()))));
    assert_eq!(iden("f3"), Ok(("", Expr::Iden("f3".to_string()))));
    // TODO test contents of errors
    assert!(iden("33").is_err());
    assert!(iden("3f").is_err());
    assert!(iden("").is_err());
}

#[test]
fn parse_ui64() {
    assert_eq!(uint64("42"), Ok(("", Expr::UInt64(42))));
    assert_eq!(uint64("0"), Ok(("", Expr::UInt64(0))));
    assert_eq!(uint64("042"), Ok(("", Expr::UInt64(42))));
    assert_eq!(uint64("00"), Ok(("", Expr::UInt64(0))));
    assert!(uint64("").is_err());
    assert!(uint64("foo").is_err());
    assert!(uint64("-42").is_err());
    // TODO:
    // test for leftovers after '4 2', '4.2'
}

#[test]
fn parse_str() {
    assert_eq!(str("\"hello\""), Ok(("", Expr::Str("hello".to_string()))));
}

#[test]
fn parse_paren() {
    assert_eq!(paren("(42)"), Ok(("", Expr::UInt64(42))));
}

#[test]
fn parse_simple_expr() {
    assert_eq!(expr("foo"), Ok(("", Expr::Iden("foo".to_string()))));
    assert_eq!(expr("42"), Ok(("", Expr::UInt64(42))));
}

#[test]
fn parse_binop() {
    let res = binop("foo + 42");
    match res {
        Ok(("", Expr::BinOp(op, e1, e2))) => {
            assert_eq!(op, BinOp::Add);
            assert_eq!(*e1, Expr::Iden("foo".to_string()));
            assert_eq!(*e2, Expr::UInt64(42));
        }
        _ => assert!(false),
    };
}

#[test]
fn parse_apply() {
    let res = apply("foo 42");
    match res {
        Ok(("", Expr::Apply(f, _e2))) => {
            assert_eq!(f, "foo".to_string());
            //assert_eq!(*e2, Expr::UInt64(42));
            //TODO: test Vec contents
        }
        _ => assert!(false),
    };
}

#[test]
fn parse_cond() {
    let c = expr("if foo then 42 else 43");
    match c {
        Ok(("", Expr::Cond(p, b1, b2))) => {
            assert_eq!(*p, Expr::Iden("foo".to_string()));
            assert_eq!(*b1, Expr::UInt64(42));
            assert_eq!(*b2, Expr::UInt64(43));
        }
        _ => { println!("{:?}", c); assert!(false) },
    }

    assert!(expr("if foo").is_err());
    assert!(expr("if foo then 42").is_err());
    assert!(expr("if then 42 else 42").is_err());
}

#[test]
fn parse_assign() {
    let a = expr("let x = 42");
    match a {
        Ok(("", Expr::Assign(iden, expr))) => {
            assert_eq!(iden, "x".to_string());
            assert_eq!(*expr, Expr::UInt64(42));
        }
        _ => { println!("{:?}", a); assert!(false) }
    }
}
