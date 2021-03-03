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

use crate::ast::{BinOp, Expr};

pub mod cmd {
}

pub mod expr {
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

pub fn apply(input: &str) -> IResult<&str, Expr> {
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

pub fn expr(input: &str) -> IResult<&str, Expr> {
    delimited(multispace0, alt((assign, cond, apply, binop, single)), multispace0)(input)
}
