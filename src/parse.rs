extern crate nom;
use crate::ast::{Arg, Stmt};
use crate::lex::{lex, Lexemes, Tok, Toks};
use crate::{err, Mode};
use nom::{
    bytes::complete::tag,
    combinator::map,
    IResult, InputIter,
};

pub mod cmd {
    use crate::{
        ast::{Arg, Cmd},
        err,
        lex::{Lexemes, Tok, Toks},
    };
    use nom::{
        branch::alt,
        bytes::complete::{tag, take},
        multi::many0,
        IResult,
    };

    fn word(input: Lexemes) -> IResult<Lexemes, String, err::Err<Lexemes>> {
        let (rest, lx) = take(1usize)(input)?;
        match &lx.lxs[0].tok {
            Tok::Word(w) => Ok((rest, w.clone())),
            _ => Err(nom::Err::Error(err::Err::NotWord(lx))),
        }
    }

    fn expr_arg(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        let (input, _) = tag(Toks::new(&[Tok::OpenModeToggle]))(input)?;
        let (input, e) = crate::parse::expr::expr(input)?;
        let (input, _) = tag(Toks::new(&[Tok::CloseParen]))(input)?;
        Ok((input, Arg::Rec(Box::new(e))))
    }

    fn cmd_arg(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        let (rest, w) = word(input)?;
        Ok((rest, Arg::Raw(w)))
    }

    fn arg(input: Lexemes) -> IResult<Lexemes, Arg, err::Err<Lexemes>> {
        alt((expr_arg, cmd_arg))(input)
    }

    pub fn cmd(input: Lexemes) -> IResult<Lexemes, Cmd, err::Err<Lexemes>> {
        let (input, cmd) = word(input)?;
        let (input, args) = many0(arg)(input)?;
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
        ast::{Expr, Single},
        err,
        lex::{Lexemes, Op, Tok, Toks},
    };
    use nom::{
        branch::alt,
        bytes::complete::{tag, take},
        IResult,
    };

    fn tok(input: Lexemes) -> IResult<Lexemes, &Tok, err::Err<Lexemes>> {
        let (input, lx) = take(1usize)(input)?;
        Ok((input, &lx.lxs[0].tok))
    }

    fn not_int_error(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (_, lx) = take(1usize)(input)?;
        Err(nom::Err::Error(err::Err::NotInt(lx)))
    }

    fn integer(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        match tok(input)? {
            (input, Tok::IntLit(i)) => Ok((input, Single::Integer(*i))),
            _ => not_int_error(input),
        }
    }

    fn not_str_error(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (_, lx) = take(1usize)(input)?;
        Err(nom::Err::Error(err::Err::NotStr(lx)))
    }

    fn str(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        match tok(input)? {
            (input, Tok::StrLit(s)) => Ok((input, Single::Str((*s).to_string()))),
            _ => not_str_error(input),
        }
    }

    fn not_iden_error(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        let (_, lx) = take(1usize)(input)?;
        Err(nom::Err::Error(err::Err::NotIden(lx)))
    }

    fn iden(input: Lexemes) -> IResult<Lexemes, Single, err::Err<Lexemes>> {
        match tok(input)? {
            (input, Tok::Iden(s)) => Ok((input, Single::Iden((*s).to_string()))),
            _ => not_iden_error(input),
        }
    }

    fn assign(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        let (input, iden) = iden(input)?;
        let iden_str = match iden {
            Single::Iden(s) => s,
            _ => "".to_string(), // TODO unreachable..
        };
        let (input, _) = tag(Toks::new(&[Tok::Op(Op::Assign)]))(input)?;
        let (input, e) = expr(input)?;
        // TODO: make Assign nest an Iden?
        Ok((input, Expr::Assign(iden_str, Box::new(e))))
    }

    fn single(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        let (input, s) = alt((integer, str, iden))(input)?;
        Ok((input, Expr::Single(s)))
    }

    fn not_op_error(input: Lexemes) -> IResult<Lexemes, Op, err::Err<Lexemes>> {
        let (_, lx) = take(1usize)(input)?;
        Err(nom::Err::Error(err::Err::NotOp(lx)))
    }

    fn op(input: Lexemes) -> IResult<Lexemes, Op, err::Err<Lexemes>> {
        match tok(input)? {
            (input, Tok::Op(o)) => Ok((input, *o)),
            _ => not_op_error(input),
        }
    }

    fn binop(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        let (input, e1) = single(input)?;
        let (input, op) = op(input)?;
        let (input, e2) = expr(input)?;
        Ok((input, Expr::BinOp(op, Box::new(e1), Box::new(e2))))
    }

    pub fn expr(input: Lexemes) -> IResult<Lexemes, Expr, err::Err<Lexemes>> {
        alt((assign, binop, single))(input)
    }
}

pub fn parse<'a, 'b>(
    input: Lexemes<'a>,
    mode: &'b Mode,
) -> IResult<Lexemes<'a>, Stmt, err::Err<Lexemes<'a>>> {
    let (input, ret) = match mode {
        Mode::Shell => map(cmd::cmd, |c| Stmt::Cmd(c))(input),
        Mode::Repl => map(expr::expr, |e| Stmt::Expr(e))(input),
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
