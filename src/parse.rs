extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric0, anychar, digit1, multispace0, multispace1, one_of},
    combinator::{map_res, recognize, verify},
    multi::many0,
    sequence::{delimited, tuple},
    AsChar, IResult,
};

use crate::ast::{BinOp, Expr, Mode, Single, Special, Stmt};

pub mod cmd {
    use crate::ast::{Arg, Cmd, Stmt};
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alphanumeric1, multispace0, multispace1},
        multi::separated_list0,
        IResult,
    };

    pub fn cmd_arg(input: &str) -> IResult<&str, Arg> {
        let (input, a) = alphanumeric1(input)?;
        Ok((input, Arg::Raw(a.to_string())))
    }

    pub fn expr_arg(input: &str) -> IResult<&str, Arg> {
        let (input, _) = tag("$(")(input)?;
        let (input, e) = crate::parse::expr(input)?;
        let (input, _) = tag(")")(input)?;
        Ok((input, Arg::Rec(Box::new(e))))
    }

    pub fn arg(input: &str) -> IResult<&str, Arg> {
        alt((cmd_arg, expr_arg))(input)
    }

    pub fn cmd(input: &str) -> IResult<&str, Stmt> {
        let (input, f) = alphanumeric1(input)?;
        let (input, _) = multispace0(input)?;
        let (input, v) = separated_list0(multispace1, arg)(input)?;
        let (input, _) = multispace0(input)?;
        Ok((
            input,
            Stmt::Cmd(Cmd {
                cmd: f.to_string(),
                args: v,
            }),
        ))
    }
}

pub mod expr {}

fn whole_num(input: &str) -> IResult<&str, Expr> {
    let (input, u) = map_res(digit1, |ds: &str| u64::from_str_radix(&ds, 10))(input)?;
    Ok((input, Expr::Single(Single::Whole(u))))
}

// TODO: integer

fn str(input: &str) -> IResult<&str, Expr> {
    // TODO: decide what characters you want in string literals
    // TODO: escape codes
    let (input, s) = delimited(tag("\""), alphanumeric0, tag("\""))(input)?;
    Ok((input, Expr::Single(Single::Str(s.to_string()))))
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
    Ok((input, Expr::Single(Single::Iden(id.to_string()))))
}

fn paren(input: &str) -> IResult<&str, Expr> {
    let (input, inner) = delimited(tag("("), expr, tag(")"))(input)?;
    Ok((input, Expr::Single(Single::Paren(Box::new(inner)))))
}

fn single(input: &str) -> IResult<&str, Expr> {
    alt((whole_num, iden, str, paren))(input)
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

fn cmd(input: &str) -> IResult<&str, Expr> {
    let (input, _) = tag("$(")(input)?;
    let (input, c) = crate::parse::cmd::cmd(input)?;
    // TODO should really be "match command or Err"
    let ret = match c {
        Stmt::Cmd(c) => Expr::Cmd(c),
        Stmt::Expr(e) => e,
        Stmt::Special(_s) => Expr::Single(Single::Str("".to_string())),
    };
    let (input, _) = tag(")")(input)?;
    Ok((input, ret))
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
        Expr::Single(Single::Iden(name)) => name,
        _ => unreachable!(),
    };
    let (input, _eq) = tag("=")(input)?;
    let (input, expr) = expr(input)?;
    Ok((input, Expr::Assign(name, Box::new(expr))))
}

fn help(input: &str) -> IResult<&str, Special> {
    let (input, _) = alt((tag("?"), tag("help"), tag(":h")))(input)?;
    Ok((input, Special::Help))
}

fn quit(input: &str) -> IResult<&str, Special> {
    let (input, _) = alt((tag("quit"), tag(":q")))(input)?;
    Ok((input, Special::Quit))
}

fn toggle_mode(input: &str) -> IResult<&str, Option<Mode>> {
    let (input, _) = alt((tag("mode"), tag(":m")))(input)?;
    Ok((input, None))
}

fn cmd_mode(input: &str) -> IResult<&str, Option<Mode>> {
    let (input, _) = tag("cmd")(input)?;
    Ok((input, Some(Mode::Cmd)))
}

fn expr_mode(input: &str) -> IResult<&str, Option<Mode>> {
    let (input, _) = tag("expr")(input)?;
    Ok((input, Some(Mode::Expr)))
}

fn mode(input: &str) -> IResult<&str, Special> {
    let (input, m) = alt((toggle_mode, cmd_mode, expr_mode))(input)?;
    Ok((input, Special::Mode(m)))
}

fn special(input: &str) -> IResult<&str, Stmt> {
    let (input, s) = alt((help, mode, quit))(input)?;
    Ok((input, Stmt::Special(s)))
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    delimited(
        multispace0,
        alt((assign, cond, cmd, binop, single)),
        multispace0,
    )(input)
}

fn expr_stmt(input: &str) -> IResult<&str, Stmt> {
    let (input, e) = expr(input)?;
    Ok((input, Stmt::Expr(e)))
}

pub fn stmt<'a, 'b>(input: &'a str, mode: &'b Mode) -> IResult<&'a str, Stmt> {
    let (input, _) = multispace0(input)?;
    let (input, ret) = match mode {
        Mode::Cmd => alt((special, cmd::cmd))(input),
        Mode::Expr => alt((special, expr_stmt))(input),
    }?;
    let (input, _) = multispace0(input)?;
    Ok((input, ret))
}
