extern crate nom;
use crate::ast::{Arg, Stmt};
use crate::lex::{lex, Lexemes, Tok, Toks};
use crate::{err, Mode};
use nom::{
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::{all_consuming, map},
    IResult, InputIter,
};
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
    use crate::{
        ast::{Arg, Cmd, Stmt},
        err,
        lex::{Lexemes, Tok, Toks},
    };
    use nom::{
        branch::alt,
        bytes::complete::{tag, take, take_while1},
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

    fn word(input: Lexemes) -> IResult<Lexemes, String, err::Err<Lexemes>> {
        let (rest, lx) = take(1usize)(input)?;
        match &lx.lxs[0].tok {
            Tok::Word(w) => Ok((rest, w.clone())),
            _ => Err(nom::Err::Error(err::Err::NotWord(lx))),
        }
    }

    fn expr_arg2(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        let (input, _) = tag(Toks::new(&[Tok::OpenModeToggle]))(input)?;
        let (input, e) = crate::parse::expr::expr2(input)?;
        let (input, _) = tag(Toks::new(&[Tok::CloseParen]))(input)?;
        Ok((input, Arg::Rec(Box::new(e))))
    }

    fn cmd_arg2(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        let (rest, w) = word(input)?;
        Ok((rest, Arg::Raw(w)))
    }

    fn arg2(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        alt((expr_arg2, cmd_arg2))(input)
    }

    pub fn cmd2(input: Lexemes) -> IResult<Lexemes, Cmd, err::Err<Lexemes>> {
        let (input, cmd) = word(input)?;
        let (input, args) = many0(arg2)(input)?;
        Ok((
            input,
            Cmd {
                cmd: cmd,
                args: args,
            },
        ))
    }
}

pub mod expr {
    use crate::{
        ast::{BinOp, Expr, Single, Stmt},
        err,
        lex::{Lexemes, Op, Tok, Toks},
    };
    use nom::{
        branch::alt,
        bytes::complete::{tag, take},
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

    fn integer2(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (input, lx) = take(1usize)(input)?;
        match &lx.lxs[0].tok {
            Tok::IntLit(i) => Ok((input, Single::Integer(*i))),
            _ => Err(nom::Err::Error(err::Err::NotInt(lx))),
        }
    }

    fn str2(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (input, lx) = take(1usize)(input)?;
        match &lx.lxs[0].tok {
            Tok::StrLit(s) => Ok((input, Single::Str((*s).to_string()))),
            _ => Err(nom::Err::Error(err::Err::NotStr(lx))),
        }
    }

    fn iden2(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (input, lx) = take(1usize)(input)?;
        match &lx.lxs[0].tok {
            Tok::Iden(s) => Ok((input, Single::Iden((*s).to_string()))),
            _ => Err(nom::Err::Error(err::Err::NotIden(lx))),
        }
    }

    fn assign2(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        let (input, iden) = iden2(input)?;
        let iden_str = match iden {
            Single::Iden(s) => s,
            _ => "".to_string(), // TODO unreachable..
        };
        let (input, _) = tag(Toks::new(&[Tok::Op(Op::Assign)]))(input)?;
        let (input, e) = expr2(input)?;
        // TODO: make Assign nest an Iden?
        Ok((input, Expr::Assign(iden_str, Box::new(e))))
    }

    fn single2(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        let (input, s) = alt((integer2, str2, iden2))(input)?;
        Ok((input, Expr::Single(s)))
    }

    pub fn expr2(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        alt((assign2, single2))(input)
    }
}

pub fn parse<'a, 'b>(
    input: Lexemes<'a>,
    mode: &'b Mode,
) -> IResult<Lexemes<'a>, Stmt, err::Err<Lexemes<'a>>> {
    let (input, ret) = match mode {
        Mode::Shell => map(cmd::cmd2, |c| Stmt::Cmd(c))(input),
        Mode::Repl => map(expr::expr2, |e| Stmt::Expr(e))(input),
    }?;
    // TODO: all_consuming or equivalent
    Ok((input, ret))
}

#[cfg(test)]
fn tok_tag(input: Lexemes, tok: Tok) -> IResult<Lexemes, Lexemes, err::Err<Lexemes>> {
    tag(Toks::new(&[tok]))(input)
}

#[test]
fn tok_tag_shell_word() {
    let mut mode = Mode::Shell;
    let lx = match lex("ls -l foo", &mut mode) {
        Ok((_, lx)) => lx,
        e => panic!("bad lex {:?}", e),
    };
    let lxs = Lexemes::new(&lx[..]);
    let expected = Tok::Word("ls".to_string());
    match tok_tag(lxs, expected.clone()) {
        Ok((rest, tagged)) => {
            assert_eq!(2, rest.lxs.len());
            assert_eq!(rest.lxs[0].tok, Tok::Word("-l".to_string()));
            assert_eq!(rest.lxs[1].tok, Tok::Word("foo".to_string()));
            assert_eq!(1, tagged.lxs.len());
            assert_eq!(tagged.lxs[0].tok, expected);
        }
        _e => {
            panic!("wrong token!");
            // TODO test error message quality?
        }
    }
}

#[test]
fn parse_simple_cmd() {
    let mut mode = Mode::Shell;
    let lx = match lex("ls -l foo", &mut mode) {
        Ok((_, lx)) => lx,
        e => panic!("bad lex {:?}", e),
    };
    let lxs = Lexemes::new(&lx[..]);
    let stmt = parse(lxs, &mut mode);
    match stmt {
        Ok((rest, Stmt::Cmd(cmd))) => {
            assert_eq!(rest.lxs.len(), 0);
            assert_eq!(cmd.cmd, "ls".to_string());
            assert_eq!(
                cmd.args,
                vec![Arg::Raw("-l".to_string()), Arg::Raw("foo".to_string())]
            );
            println!("{:?}", cmd);
        }
        e => panic!("bad parse {:?}", e),
    }
}
