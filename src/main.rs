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

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    UInt64(u64),
    Str(String),
    Iden(String),
    Apply(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Err {
    Read,
    Parse,
    Eval,
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
    !(s == "if" || s == "then" || s == "else")
}

fn iden(input: &str) -> IResult<&str, Expr> {
    println!("iden? {}", input);
    let (input, id) = verify(
        recognize(tuple((
            verify(anychar, |c: &char| (*c).is_alpha() || *c == '_'),
            many0(verify(anychar, iden_char)),
        ))),
        valid_iden,
    )(input)?;
    println!("iden! input: {}, id: {}", input, id);
    Ok((input, Expr::Iden(id.to_string())))
}

fn paren(input: &str) -> IResult<&str, Expr> {
    println!("paren? {}", input);
    let (input, inner) = delimited(tag("("), expr, tag(")"))(input)?;
    println!("paren! {:?}", inner);
    Ok((input, inner))
}

fn single(input: &str) -> IResult<&str, Expr> {
    println!("single? {}", input);
    let (input, e) = alt((uint64, iden, str, paren))(input)?;
    println!("single! {}, {:?}", input, e);
    Ok((input, e))
}

fn binop(input: &str) -> IResult<&str, Expr> {
    println!("binop? {}", input);
    let (input, (e1, _, op, _, e2)) =
        tuple((single, multispace0, one_of("+-*/"), multispace0, single))(input)?;
    println!("binop! {}", input);
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
    println!("apply? {}", input);
    let (input, (f, _, v)) =
        tuple((iden, multispace1, separated_list1(multispace1, single)))(input)?;
    println!("apply!");
    Ok((input, Expr::Apply(Box::new(f), v)))
}

fn cond(input: &str) -> IResult<&str, Expr> {
    println!("cond? {}", input);
    let (input, _if) = tag("if")(input)?;
    let (input, pred) = expr(input)?;
    println!("cond: if {:?}", pred);
    let (input, _then) = tag("then")(input)?;
    let (input, br1) = expr(input)?;
    println!("cond: if {:?} then {:?}", pred, br1);
    let (input, _else) = tag("else")(input)?;
    let (input, br2) = expr(input)?;
    println!("cond: if {:?} then {:?} else {:?}", pred, br1, br2);
    Ok((
        input,
        Expr::Cond(Box::new(pred), Box::new(br1), Box::new(br2)),
    ))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    delimited(multispace0, alt((cond, apply, binop, single)), multispace0)(input)
}

// TODO: what is the return type? some kind of value type...
fn eval(e: Expr) -> String {
    println!("EVAL! {:?}", e);
    match e {
        Expr::UInt64(i) => format!("{}:u64", i),
        Expr::Str(s) => format!("{}:str", s),
        Expr::Iden(n) => format!("iden: {}", n), // It should be an empty apply!?!
        Expr::Apply(_f, _args) => format!("placeholder. apply"),
        Expr::BinOp(_op, _e1, _e2) => format!("placeholder. binop"),
        Expr::Cond(_pred, _br1, _br2) => format!("placeholder. cond"),
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

fn read_eval() -> Result<String, Err> {
    let line = read()?;
    let expr = parse(&line)?;
    Ok(eval(expr))
}

fn repl() {
    loop {
        print!("$ ");
        std::io::stdout().flush();
        match read_eval() {
            Ok(s) => println!("{}", s),
            Err(e) => {
                println!("{:?}", e);
                break;
            }
        }
    }
}

fn main() {
    repl();
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
        Ok(("", Expr::Apply(e1, _e2))) => {
            assert_eq!(*e1, Expr::Iden("foo".to_string()));
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
