extern crate nom;
use crate::ast::{Mode, Special, Stmt};
use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, combinator::all_consuming,
    error::context, IResult,
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

pub mod err {
    use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};

    #[derive(Debug)]
    pub enum Err<I> {
        Int(I, String),
        Nom(I, ErrorKind),
    }

    impl<I> ParseError<I> for Err<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            Err::Nom(input, kind)
        }
        fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
            other
        }
    }

    // TODO
    // or
    // from_char

    impl<I> FromExternalError<I, std::num::ParseIntError> for Err<I> {
        fn from_external_error(input: I, _kind: ErrorKind, e: std::num::ParseIntError) -> Self {
            Err::Int(input, format!("{}", e))
        }
    }

    impl<I> ContextError<I> for Err<I> {
        fn add_context(_input: I, ctx: &'static str, other: Self) -> Self {
            println!("add_context: {}", ctx);
            other
        }
    }
}

pub mod lex {
    use crate::parse::err;
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_till},
        character::complete::{alphanumeric0, anychar, digit1, multispace0},
        combinator::{map_parser, map_res, not, peek, recognize, verify},
        multi::many0,
        sequence::{delimited, tuple},
        AsChar, IResult,
    };

    #[derive(Debug, PartialEq)]
    pub enum Op {
        Add,
        Sub,
        Mul,
        Div,
        Assign,
        Eq,
        Neq,
        Gt,
        Gte,
        Lt,
        Lte,
        Not,
        And,
        Or,
        Xor,
        BitAnd,
        BitOr,
    }

    // TODO store position
    #[derive(Debug, PartialEq)]
    pub enum Tok {
        OpenModeToggle,
        OpenParen,
        CloseParen,
        Op(Op),
        Let,
        If,
        Then,
        Else,
        IntLit(i128),
        StrLit(String),
        Iden(String),
        Raw(String),
    }

    fn open_mode(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag("$(")(input)?;
        crate::parse::inc_toggle_depth();
        Ok((input, Tok::OpenModeToggle))
    }

    fn open_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag("(")(input)?;
        crate::parse::inc_toggle_depth();
        Ok((input, Tok::OpenParen))
    }

    fn close_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag(")")(input)?;
        crate::parse::dec_toggle_depth();
        Ok((input, Tok::CloseParen))
    }

    fn add(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("+")(input)?;
        Ok((input, Op::Add))
    }

    fn sub(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("-")(input)?;
        Ok((input, Op::Sub))
    }

    fn mul(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("*")(input)?;
        Ok((input, Op::Mul))
    }

    fn div(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("/")(input)?;
        Ok((input, Op::Div))
    }

    fn op_not(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("!")(input)?;
        Ok((input, Op::Not))
    }

    fn eq(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("==")(input)?;
        Ok((input, Op::Eq))
    }

    fn neq(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("!=")(input)?;
        Ok((input, Op::Neq))
    }

    fn gte(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag(">=")(input)?;
        Ok((input, Op::Gte))
    }

    fn lte(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("<=")(input)?;
        Ok((input, Op::Lte))
    }

    fn gt(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag(">")(input)?;
        Ok((input, Op::Gt))
    }

    fn lt(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("<")(input)?;
        Ok((input, Op::Lt))
    }

    fn and(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("&&")(input)?;
        Ok((input, Op::And))
    }

    fn or(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("||")(input)?;
        Ok((input, Op::Or))
    }

    fn xor(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("^")(input)?;
        Ok((input, Op::Xor))
    }

    fn bitand(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("&")(input)?;
        Ok((input, Op::BitAnd))
    }

    fn bitor(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("|")(input)?;
        Ok((input, Op::BitOr))
    }

    fn assign(input: &str) -> IResult<&str, Op, err::Err<&str>> {
        let (input, _) = tag("=")(input)?;
        Ok((input, Op::Assign))
    }

    fn op(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, o) = alt((
            add, sub, mul, div, neq, op_not, eq, gte, gt, lte, lt, and, or, xor, bitand, bitor, assign
        ))(input)?;
        Ok((input, Tok::Op(o)))
    }

    fn lex_if(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag("if")(input)?;
        Ok((input, Tok::If))
    }

    fn lex_then(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag("then")(input)?;
        Ok((input, Tok::Then))
    }

    fn lex_else(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, _) = tag("else")(input)?;
        Ok((input, Tok::Else))
    }

    fn keyword(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        alt((
            lex_if,
            lex_then,
            lex_else,
        ))(input)
    }

    fn sep(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        alt((
                open_mode,
                open_paren,
                close_paren,
        ))(input)
    }

    fn int_lit(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, n) =
            map_res(digit1, |ds: &str| i128::from_str_radix(&ds, 10))(input)?;
        Ok((input, Tok::IntLit(n)))
    }

    fn str_lit(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        // TODO: decide what characters you want in string literals
        // TODO: escape codes
        let (input, s) = delimited(tag("\""), alphanumeric0, tag("\""))(input)?;
        Ok((input, Tok::StrLit(s.to_string())))
    }

    fn lit(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        alt((int_lit, str_lit))(input)
    }

    fn iden_char(c: &char) -> bool {
        (*c).is_alphanum() || *c == '_'
    }

    fn valid_iden(s: &str) -> bool {
        match not(keyword)(s) {
            Ok(_) => true,
            _ => false
        }
    }

    fn iden(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, id) =
            verify(
                recognize(tuple((
                    verify(anychar, |c: &char| (*c).is_alpha() || *c == '_'),
                    many0(verify(anychar, iden_char)),
                ))),
                valid_iden,
            )(input)?;
        Ok((input, Tok::Iden(id.to_string())))
    }


    fn raw(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        println!("tok: {}", input);
        let (input, _) = peek(anychar)(input)?;
        Ok(("", Tok::Raw(input.to_string())))
    }

    fn end_of_token(c: char) -> bool {
        // TODO vec deque of positions, not just a depth counter
        (c == ' ')
            || (c == '\t')
            || (c == '\n')
            || (c == '\r')
            || (c == '+')
            || (c == '-')
            || (c == '*')
            || (c == '/')
            || (c == '!')
            || (c == '=')
            || (c == '^')
            || (c == '&')
            || (c == '|')
            || (c == ')')
            || (c == '(')
            || (c == '$')
            || (c == '"')
            || (c == '<')
            || (c == '>')
    }

    fn lex_one(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        println!("one: {}", input);
        let (input, _) = multispace0(input)?;
        let (input, tok) = alt((keyword, sep, op, iden, lit, map_parser(take_till(end_of_token), raw)))(input)?;
        println!("one tok: {:?}", tok);
        let (input, _) = multispace0(input)?;
        Ok((input, tok))
    }

    pub fn lex(input: &str) -> IResult<&str, Vec<Tok>, err::Err<&str>> {
        many0(lex_one)(input)
        // end of lex -> all parens closed
        // Track a stack of open parens instead of a counter for a better error message
    }

    #[test]
    fn test_num() {
        match lex("42") {
            Ok((_, actual)) => assert_eq!(actual, vec![Tok::IntLit(42)]),
            e => panic!("bad lex {:?}", e),
        }
    }

    #[cfg(test)]
    fn do_lex_test(inputs: Vec<&str>, expected: Vec<Tok>) {
        for input in inputs.into_iter() {
            match lex(input) {
                Ok((i, actual)) => {
                    assert_eq!(i, "");
                    assert_eq!(expected, actual)
                },
                e => panic!("bad lex {:?}", e),
            }
        }
    }

    #[test]
    fn test_if_then_else() {
        let inputs = vec![
            "if pred then b1 else b2"
        ];
        let toks = vec![
            Tok::If,
            Tok::Iden("pred".to_string()),
            Tok::Then,
            Tok::Iden("b1".to_string()),
            Tok::Else,
            Tok::Iden("b2".to_string()),
        ];
        do_lex_test(inputs, toks);
    }

    #[test]
    fn test_assign() {
        let inputs = vec!["x = expr"];
        let toks = vec![
            Tok::Iden("x".to_string()),
            Tok::Op(Op::Assign),
            Tok::Iden("expr".to_string()),
        ];
        do_lex_test(inputs, toks);
    }

    #[cfg(test)]
    fn do_binop_test(op_str: &str, op: Op) {
        let expected = vec![
            Tok::IntLit(42),
            Tok::Op(op),
            Tok::IntLit(42),
        ];
        let inputs = vec![
            format!("42 {} 42", op_str),
            format!("42  {}  42", op_str),
            format!("42{}42", op_str),
            format!("42{} 42", op_str),
            format!("42 {}42", op_str),
        ];
        // Can't use do_lex_test because of String vs &str
        for input in inputs.into_iter() {
            match lex(&input) {
                Ok((i, actual)) => {
                    assert_eq!(i, "");
                    assert_eq!(expected, actual)
                },
                e => panic!("bad lex {:?}", e),
            }
        }
    }

    #[test]
    fn test_binop() {
        do_binop_test("+", Op::Add);
        do_binop_test("-", Op::Sub);
        do_binop_test("*", Op::Mul);
        do_binop_test("/", Op::Div);
        do_binop_test(">", Op::Gt);
        do_binop_test(">=", Op::Gte);
        do_binop_test("<", Op::Lt);
        do_binop_test("<=", Op::Lte);
        do_binop_test("=", Op::Assign);
        do_binop_test("==", Op::Eq);
        do_binop_test("!=", Op::Neq);
        do_binop_test("&&", Op::And);
        do_binop_test("||", Op::Or);
        do_binop_test("&", Op::BitAnd);
        do_binop_test("|", Op::BitOr);
        do_binop_test("^", Op::Xor);
    }

    #[test]
    fn test_parens() {
        let inputs = vec![
            "((foo bar))",
            " ((foo  bar))",
            "  ((foo  bar)) ",
            "((   foo bar))",
            "( (foo bar )  )",
            "( ( foo bar   )  ) ",
        ];
        let toks = vec![
            Tok::OpenParen,
            Tok::OpenParen,
            Tok::Iden("foo".to_string()),
            Tok::Iden("bar".to_string()),
            Tok::CloseParen,
            Tok::CloseParen,
        ];
        do_lex_test(inputs, toks);
    }

    #[test]
    fn test_mode_toggle() {
        let inputs = vec![
            "$()",
            "$( )",
        ];
        let toks = vec![
            Tok::OpenModeToggle,
            Tok::CloseParen,
        ];
        do_lex_test(inputs, toks);
    }

}

pub mod cmd {
    use crate::ast::{Arg, Cmd, Stmt};
    use crate::parse::err;
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
            && (crate::parse::get_toggle_depth() == 0 || c != ')')
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
    use crate::parse::err;
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
            Stmt::Special(_s) => Expr::Single(Single::Str("".to_string())),
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

fn help(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, _) = alt((tag("?"), tag("help"), tag(":h")))(input)?;
    Ok((input, Special::Help))
}

fn quit(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, _) = alt((tag("quit"), tag(":q")))(input)?;
    Ok((input, Special::Quit))
}

fn toggle_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = alt((tag("mode"), tag(":m")))(input)?;
    Ok((input, None))
}

fn cmd_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = tag("cmd")(input)?;
    Ok((input, Some(Mode::Cmd)))
}

fn expr_mode(input: &str) -> IResult<&str, Option<Mode>, err::Err<&str>> {
    let (input, _) = tag("expr")(input)?;
    Ok((input, Some(Mode::Expr)))
}

fn mode(input: &str) -> IResult<&str, Special, err::Err<&str>> {
    let (input, m) = alt((toggle_mode, cmd_mode, expr_mode))(input)?;
    Ok((input, Special::Mode(m)))
}

fn special(input: &str) -> IResult<&str, Stmt, err::Err<&str>> {
    let (input, s) = alt((help, mode, quit))(input)?;
    Ok((input, Stmt::Special(s)))
}

fn expr_stmt(input: &str) -> IResult<&str, Stmt, err::Err<&str>> {
    let (input, e) = crate::parse::expr::expr(input)?;
    Ok((input, Stmt::Expr(e)))
}

pub fn stmt<'a, 'b>(input: &'a str, mode: &'b Mode) -> IResult<&'a str, Stmt, err::Err<&'a str>> {
    let (input, _) = multispace0(input)?;
    let (input, ret) = match mode {
        Mode::Cmd => context("cmd mode", alt((special, cmd::cmd)))(input),
        Mode::Expr => context("expr mode", alt((special, expr_stmt)))(input),
    }?;
    let (input, _) = context("stmt", all_consuming(multispace0))(input)?;
    Ok((input, ret))
}
