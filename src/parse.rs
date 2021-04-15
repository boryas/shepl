extern crate nom;
use crate::{err, Mode};
use crate::ast::Stmt;
use crate::lex::{lex, Lexemes};
use nom::{character::complete::multispace0, combinator::all_consuming, error::context, IResult, InputIter};
use std::cell::RefCell;

thread_local! {
    pub static MODE_TOGGLE_DEPTH: RefCell<u32> = RefCell::new(0);
}

fn get_toggle_depth() -> u32 {
    MODE_TOGGLE_DEPTH.with(|d| *d.borrow())
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
    use crate::err;
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        character::complete::multispace1,
        combinator::recognize,
        multi::many0,
        IResult,
    };

    fn arg_char(c: char) -> bool {
        (c != ' ')
            && (c != '\t')
            && (c != '\n')
            && (c != '\r')
            && ((crate::parse::get_toggle_depth() % 2) == 0 || c != ')')
    }

    fn cmd_arg(input: &str) -> IResult<&str, Arg, err::Err<&str>> {
        let (input, a) = take_while1(arg_char)(input)?;
        Ok((input, Arg::Raw(a.to_string())))
    }

    fn expr_arg(input: &str) -> IResult<&str, Arg, err::Err<&str>> {
        let (input, _) = tag("$(")(input)?;
        crate::parse::inc_toggle_depth();
        let (input, e) = crate::parse::expr::expr(input)?;
        let (input, _) = tag(")")(input)?;
        crate::parse::dec_toggle_depth();
        Ok((input, Arg::Rec(Box::new(e))))
    }

    fn arg(input: &str) -> IResult<&str, Arg, err::Err<&str>> {
        let (input, _) = multispace1(input)?;
        alt((expr_arg, cmd_arg))(input)
    }

    pub fn cmd(input: &str) -> IResult<&str, Stmt, err::Err<&str>> {
        let (input, f) = recognize(cmd_arg)(input)?;
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
    use crate::err;
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alphanumeric0, anychar, digit1, multispace0, multispace1, one_of},
        combinator::{map_res, opt, recognize, verify},
        error::context,
        multi::many0,
        sequence::{delimited, tuple},
        AsChar, IResult,
    };

    fn integer(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        println!("{}: int?", input);
        let (input, neg) = opt(tag("-"))(input)?;
        let (input, n) = context(
            "int",
            map_res(digit1, |ds: &str| i128::from_str_radix(&ds, 10)),
        )(input)?;
        let i = match neg {
            Some(_) => n * -1,
            None => n,
        };
        println!("int! {}: {}", input, i);
        Ok((input, Expr::Single(Single::Integer(i))))
    }

    fn str(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        // TODO: decide what characters you want in string literals
        // TODO: escape codes
        let (input, s) = context("str", delimited(tag("\""), alphanumeric0, tag("\"")))(input)?;
        Ok((input, Expr::Single(Single::Str(s.to_string()))))
    }

    fn iden_char(c: &char) -> bool {
        (*c).is_alphanum() || *c == '_'
    }

    fn valid_iden(s: &str) -> bool {
        // TODO: use a set
        !(s == "if" || s == "then" || s == "else" || s == "let")
    }

    fn iden(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        let (input, id) = context(
            "iden",
            verify(
                recognize(tuple((
                    verify(anychar, |c: &char| (*c).is_alpha() || *c == '_'),
                    many0(verify(anychar, iden_char)),
                ))),
                valid_iden,
            ),
        )(input)?;
        Ok((input, Expr::Single(Single::Iden(id.to_string()))))
    }

    fn paren(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        let (input, inner) = context("paren", delimited(tag("("), expr, tag(")")))(input)?;
        Ok((input, Expr::Single(Single::Paren(Box::new(inner)))))
    }

    fn single(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        context("single", alt((integer, iden, str, paren)))(input)
    }

    fn binop(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        let (input, (e1, _, op, _, e2)) = context(
            "binop",
            tuple((single, multispace0, one_of("+-*/"), multispace0, single)),
        )(input)?;
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

    fn cmd(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        let (input, _) = context("cmd expr", tag("$("))(input)?;
        crate::parse::inc_toggle_depth();
        let (input, c) = context("cmd expr", crate::parse::cmd::cmd)(input)?;
        // TODO should really be "match command or Err"
        let ret = match c {
            Stmt::Cmd(c) => Expr::Cmd(c),
            Stmt::Expr(e) => e,
        };
        let (input, _) = context("cmd expr", tag(")"))(input)?;
        crate::parse::dec_toggle_depth();
        Ok((input, ret))
    }

    fn cond(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
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

    fn assign(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
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

    pub fn expr(input: &str) -> IResult<&str, Expr, err::Err<&str>> {
        context(
            "expr",
            delimited(
                multispace0,
                alt((
                    context("assign", assign),
                    context("cond", cond),
                    cmd,
                    binop,
                    single,
                )),
                multispace0,
            ),
        )(input)
    }
}

fn expr_stmt(input: &str) -> IResult<&str, Stmt, err::Err<&str>> {
    let (input, e) = crate::parse::expr::expr(input)?;
    Ok((input, Stmt::Expr(e)))
}

pub fn stmt<'a, 'b>(input: &'a str, mode: &'b Mode) -> IResult<&'a str, Stmt, err::Err<&'a str>> {
    let (input, _) = multispace0(input)?;
    let (input, ret) = match mode {
        Mode::Shell => context("cmd mode", cmd::cmd)(input),
        Mode::Repl => context("expr mode", expr_stmt)(input),
    }?;
    let (input, _) = context("stmt", all_consuming(multispace0))(input)?;
    Ok((input, ret))
}

pub fn parse<'a, 'b>(
    input: Lexemes<'a>,
    _mode: &'b Mode,
) -> IResult<Lexemes<'a>, Stmt, err::Err<Lexemes<'a>>> {
    // TODO wat
    for lx in input.iter_elements() {
        println!("Parse sees lexeme {:?}", lx);
    }
    Err(nom::Err::Error(err::Err::Unimp))
}

fn tok_tag<'a>(input: Lexemes<'a>, tok: Tok) -> IResult<Lexemes<'a>, Tok, err::Err<Lexemes<'a>>>{
    let input = input.take(1);
    if input.lxs[0].tok == tok {
        tok
    }
    Err(nom::Err::Error(err::Err::Nom(input, nom:Err::TagError)))
}

#[test]
fn dummy_parse() {
    let mut mode = Mode::Shell;
    let lx = match lex("ls -l foo", &mut mode) {
        Ok((input, lx)) => lx,
        e => panic!("bad lex {:?}", e),
    };
    let lxs = Lexemes::new(&lx[..]);
    parse(lxs, &mut mode);
}
