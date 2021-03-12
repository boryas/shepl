extern crate nom;
use crate::ast::{Mode, Special, Stmt};
use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::multispace0,
        IResult,
};
use std::cell::RefCell;

thread_local! {
    pub static MODE_TOGGLE_DEPTH: RefCell<u32> = RefCell::new(0);
}

fn get_toggle_depth() -> u32 {
    MODE_TOGGLE_DEPTH.with(|d| {
        *d.borrow()
    })
}

fn inc_toggle_depth() {
    MODE_TOGGLE_DEPTH.with(|d| {
        *d.borrow_mut() += 1;
    })
}

fn dec_toggle_depth() {
    MODE_TOGGLE_DEPTH.with(|d| {
        *d.borrow_mut() -= 1;
    })
}
pub mod cmd {
    use crate::ast::{Arg, Cmd, Stmt};
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        character::complete::{alphanumeric1, multispace1},
        multi::{many0},
        IResult,
    };

    fn arg_char(c: char) -> bool {
        (c != ' ') && (c != '\t') && (c != '\n') && (c != '\r')
            && (crate::parse::get_toggle_depth() == 0 || c != ')')
    }

    fn cmd_arg(input: &str) -> IResult<&str, Arg> {
        let (input, a) = take_while1(arg_char)(input)?;
        Ok((input, Arg::Raw(a.to_string())))
    }

    fn expr_arg(input: &str) -> IResult<&str, Arg> {
        let (input, _) = tag("$(")(input)?;
        crate::parse::inc_toggle_depth();
        let (input, e) = crate::parse::expr::expr(input)?;
        let (input, _) = tag(")")(input)?;
        crate::parse::dec_toggle_depth();
        Ok((input, Arg::Rec(Box::new(e))))
    }

    fn arg(input: &str) -> IResult<&str, Arg> {
        let (input, _) = multispace1(input)?;
        alt((expr_arg, cmd_arg))(input)
    }

    pub fn cmd(input: &str) -> IResult<&str, Stmt> {
        let (input, f) = alphanumeric1(input)?;
        let (input, v) = many0(arg)(input)?;
        Ok((
            input,
            Stmt::Cmd(Cmd {
                cmd: f.to_string(),
                args: v,
            }),
        ))
    }
}

pub mod expr {
    use crate::ast::{BinOp, Expr, Single, Stmt};
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alphanumeric0, anychar, digit1, multispace0, multispace1, one_of},
        combinator::{map_res, opt, recognize, verify},
        multi::many0,
        sequence::{delimited, tuple},
        AsChar,
        IResult,
    };

    fn integer(input: &str) -> IResult<&str, Expr> {
        let (input, neg) = opt(tag("-"))(input)?;
        let (input, n) = map_res(digit1, |ds: &str| i128::from_str_radix(&ds, 10))(input)?;
        let i = match neg {
            Some(_) => n * -1,
            None => n
        };
        Ok((input, Expr::Single(Single::Integer(i))))
    }

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
        alt((integer, iden, str, paren))(input)
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
        crate::parse::inc_toggle_depth();
        let (input, c) = crate::parse::cmd::cmd(input)?;
        // TODO should really be "match command or Err"
        let ret = match c {
            Stmt::Cmd(c) => Expr::Cmd(c),
            Stmt::Expr(e) => e,
            Stmt::Special(_s) => Expr::Single(Single::Str("".to_string())),
        };
        let (input, _) = tag(")")(input)?;
        crate::parse::dec_toggle_depth();
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

    pub fn expr(input: &str) -> IResult<&str, Expr> {
        delimited(
            multispace0,
            alt((assign, cond, cmd, binop, single)),
            multispace0,
        )(input)
    }
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

fn expr_stmt(input: &str) -> IResult<&str, Stmt> {
    let (input, e) = crate::parse::expr::expr(input)?;
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
