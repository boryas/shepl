extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, digit1, multispace0, multispace1, one_of},
    combinator::{map_res, recognize, verify},
    multi::{many0, separated_list1},
    sequence::{delimited, tuple},
    AsChar, IResult,
};

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    UInt64(i64),
    Iden(String),
    Apply(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
}

fn uint64(input: &str) -> IResult<&str, Expr> {
    println!("u64? {}", input);
    let (input, i) = map_res(digit1, |ds: &str| i64::from_str_radix(&ds, 10))(input)?;
    println!("u64! {}", input);
    Ok((input, Expr::UInt64(i)))
}

fn iden_char(c: &char) -> bool {
    (*c).is_alphanum() || *c == '_'
}

fn valid_iden(s: &str) -> bool {
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
    println!("iden! {}", input);
    Ok((input, Expr::Iden(id.to_string())))
}

fn paren(input: &str) -> IResult<&str, Expr> {
    println!("paren? {}", input);
    let (input, inner) = delimited(tag("("), expr, tag(")"))(input)?;
    println!("paren! {}", input);
    Ok((input, inner))
}

fn single(input: &str) -> IResult<&str, Expr> {
    println!("single? {}", input);
    let (input, e) = alt((uint64, iden, paren))(input)?;
    println!("single! {}", input);
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
    println!("apply! {}", input);
    Ok((input, Expr::Apply(Box::new(f), v)))
}

fn cond(input: &str) -> IResult<&str, Expr> {
    println!("cond? {}", input);
    let (input, (_if, _w1, pred, _w2, _then, _w3, br1, _w4, _else, _w5, br2)) = tuple((
        tag("if"),
        multispace1,
        expr,
        multispace1,
        tag("then"),
        multispace1,
        expr,
        multispace1,
        tag("else"),
        multispace1,
        expr,
    ))(input)?;
    println!("cond! {}", input);
    Ok((
        input,
        Expr::Cond(Box::new(pred), Box::new(br1), Box::new(br2)),
    ))
}

fn expr(input: &str) -> IResult<&str, Expr> {
    println!("expr? {}", input);
    let (input, e) = alt((cond, apply, binop, single))(input)?;
    println!("expr! {:?}", e);
    Ok((input, e))
}

fn main() {
    //println!("{:?}", apply("foo 42 43 44"));
    //println!("{:?}", expr("42 + 43"));
    // TODO: handle this case aka "Associativity Problem"
    //println!("{:?}", expr("42 + 43 + 44"));
    //println!("{:?}", expr("42 + (43 + 44)"));
    println!("{:?}", expr("foo"));
    println!("{:?}", expr("if 42 then 43 else 44"));
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
        _ => assert_eq!(true, false),
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
        _ => assert_eq!(true, false),
    };
}
