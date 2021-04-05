use crate::parse::err;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until},
    character::complete::{anychar, digit1, multispace0, satisfy},
    combinator::{consumed, map_parser, map_res, not, peek, recognize, verify},
    multi::many0,
    sequence::{delimited, tuple},
    AsChar, IResult,
};
use std::cell::RefCell;
use std::collections::VecDeque;

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug)]
pub struct LexPos {
    col: usize,
    ln: usize,
}

#[derive(Clone, Debug)]
pub struct Lexeme {
    tok: Tok,
    pos: LexPos,
    len: usize,
}

struct LexCtx {
    pos: LexPos,
    parens: VecDeque<Lexeme>,
}

impl LexCtx {
    fn new() -> LexCtx {
        LexCtx {
            pos: LexPos { col: 0, ln: 0 },
            parens: VecDeque::new(),
        }
    }
}

thread_local! {
    static LEX_CTX: RefCell<LexCtx> = RefCell::new(LexCtx::new());
}

fn push_paren(tok: Tok) {
    LEX_CTX.with(|ctx| {
        let lx = Lexeme {
            tok: tok,
            pos: (*ctx).borrow().pos,
            len: 2,
        };
        (*ctx).borrow_mut().parens.push_back(lx);
    })
}

fn pop_paren() {
    LEX_CTX.with(|ctx| {
        // TODO: error if no paren left
        // TODO: communicate what you popped (open paren vs open mode toggle)
        (*ctx).borrow_mut().parens.pop_back();
    })
}

pub fn ctx_reset() {
    LEX_CTX.with(|ctx| {
        *(*ctx).borrow_mut() = LexCtx::new();
    })
}

pub fn ctx_pos_forward(off: usize) {
    LEX_CTX.with(|ctx| {
        {
            let col: &mut usize = &mut (*ctx).borrow_mut().pos.col;
            *col += off;
        }
    })
}

pub fn ctx_pos_new_line() {
    LEX_CTX.with(|ctx| {
        {
            let ln: &mut usize = &mut (*ctx).borrow_mut().pos.ln;
            *ln += 1;
        }
        {
            let col: &mut usize = &mut (*ctx).borrow_mut().pos.col;
            *col = 0;
        }
    })
}

pub fn ctx_pos() -> LexPos {
    let mut pos = LexPos { col: 0, ln: 0 };
    LEX_CTX.with(|ctx| pos = (*ctx).borrow().pos);
    pos
}

fn open_mode(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag("$(")(input)?;
    push_paren(Tok::OpenModeToggle);
    Ok((input, Tok::OpenModeToggle))
}

fn open_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag("(")(input)?;
    push_paren(Tok::OpenParen);
    Ok((input, Tok::OpenParen))
}

fn close_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag(")")(input)?;
    pop_paren();
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
        add, sub, mul, div, neq, op_not, eq, gte, gt, lte, lt, and, or, xor, bitand, bitor, assign,
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
    let (input, t) = alt((lex_if, lex_then, lex_else))(input)?;
    let (input, _) = peek(satisfy(end_of_token))(input)?;
    Ok((input, t))
}

fn sep(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    alt((open_mode, open_paren, close_paren))(input)
}

fn int_lit(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, n) = map_res(digit1, |ds: &str| i128::from_str_radix(&ds, 10))(input)?;
    Ok((input, Tok::IntLit(n)))
}

fn str_lit(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    // TODO: decide what characters you want in string literals
    // TODO: escape codes instead of making them "type it"
    let (input, s) = delimited(tag("\""), take_until("\""), tag("\""))(input)?;
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
        _ => false,
    }
}

fn iden(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, id) = verify(
        recognize(tuple((
            verify(anychar, |c: &char| (*c).is_alpha() || *c == '_'),
            many0(verify(anychar, iden_char)),
        ))),
        valid_iden,
    )(input)?;
    Ok((input, Tok::Iden(id.to_string())))
}

fn raw(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = peek(anychar)(input)?;
    Ok(("", Tok::Raw(input.to_string())))
}

// TODO check a map or a pattern match...
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

pub mod expr {
    use crate::lex::{ctx_pos, ctx_pos_forward, ctx_pos_new_line, iden, keyword, lit, op, sep, Lexeme};
    use crate::parse::err;
    use nom::{branch::alt, character::complete::multispace0, combinator::consumed, IResult};

    fn update_pos(consumed: &str) {
        let mut lns = consumed.lines();
        match lns.next() {
            Some(ln) => {
                ctx_pos_forward(ln.len())
            },
            None => (),
        }
        for ln in lns {
            ctx_pos_new_line();
            ctx_pos_forward(ln.len());
        }
    }

    pub fn lex_one(input: &str) -> IResult<&str, Lexeme, err::Err<&str>> {
        let (input, w_str1) = multispace0(input)?;
        update_pos(w_str1);

        let tok_pos = ctx_pos();
        let (input, (tok_str, tok)) = consumed(alt((keyword, sep, op, iden, lit)))(input)?;
        update_pos(tok_str);

        let (input, w_str2) = multispace0(input)?;
        update_pos(w_str2);

        Ok((
            input,
            Lexeme {
                tok: tok,
                pos: tok_pos,
                len: tok_str.len(),
            },
        ))
    }
}

// TODO:
// expr vs cmd
pub fn lex(input: &str) -> IResult<&str, Vec<Lexeme>, err::Err<&str>> {
    ctx_reset();
    let mut lxs: Vec<Lexeme> = Vec::new();
    let mut inp = input;
    loop {
        let (i_tmp, (lx_str, lx)) = consumed(crate::lex::expr::lex_one)(inp)?;
        inp = i_tmp;
        if lx_str == "" {
            panic!("did not consuuuume");
        }
        lxs.push(lx);
        if inp.len() == 0 {
            break;
        }
    }

    println!("lexed: {} as {:?}", input, lxs);
    Ok(("", lxs))
}

#[cfg(test)]
fn do_lex_test(inputs: Vec<&str>, expected: Vec<Tok>) {
    for input in inputs.into_iter() {
        match lex(input) {
            Ok((i, actual)) => {
                assert_eq!(i, "");
                let mut toks: Vec<Tok> = Vec::new();
                for lx in actual {
                    toks.push(lx.tok);
                }
                assert_eq!(expected, toks)
            }
            e => panic!("bad lex {:?}", e),
        }
    }
}

#[test]
fn test_num() {
    let inputs = vec!["42"];
    let toks = vec![Tok::IntLit(42)];
    do_lex_test(inputs, toks);
}

#[test]
fn test_if_then_else() {
    let inputs = vec!["if pred then b1 else b2"];
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
fn test_keyword_vs_iden() {
    let i1 = vec!["if f"];
    let toks1 = vec![Tok::If, Tok::Iden("f".to_string())];
    do_lex_test(i1, toks1);
    let i2 = vec!["iff"];
    let toks2 = vec![Tok::Iden("iff".to_string())];
    do_lex_test(i2, toks2);
    let i3 = vec!["if(f)"];
    let toks3 = vec![
        Tok::If,
        Tok::OpenParen,
        Tok::Iden("f".to_string()),
        Tok::CloseParen,
    ];
    do_lex_test(i3, toks3);
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
    let expected = vec![Tok::IntLit(42), Tok::Op(op), Tok::IntLit(42)];
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
                let mut toks: Vec<Tok> = Vec::new();
                for lx in actual {
                    toks.push(lx.tok);
                }
                assert_eq!(expected, toks)
            }
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
    let inputs = vec!["$()", "$( )"];
    let toks = vec![Tok::OpenModeToggle, Tok::CloseParen];
    do_lex_test(inputs, toks);
}

#[test]
fn test_mix_toggle_paren() {
    let inputs1 = vec!["$(())", "$(  ())", "  $((   )   )"];
    let toks1 = vec![
        Tok::OpenModeToggle,
        Tok::OpenParen,
        Tok::CloseParen,
        Tok::CloseParen,
    ];
    do_lex_test(inputs1, toks1);
    let inputs2 = vec!["($())", "( $( ) )"];
    let toks2 = vec![
        Tok::OpenParen,
        Tok::OpenModeToggle,
        Tok::CloseParen,
        Tok::CloseParen,
    ];
    do_lex_test(inputs2, toks2);
}

#[cfg(test)]
fn do_str_lit_test(s: &str) {
    let strung = format!("\"{}\"", s);
    let input = vec![&strung[..]];
    let tok = vec![Tok::StrLit(s.to_string())];
    do_lex_test(input, tok);
}

#[test]
fn test_str_lit() {
    do_str_lit_test("foo bar   baz");
    do_str_lit_test("foo	bar   baz");
    do_str_lit_test(
        "foo bar
        baz",
    );
    do_str_lit_test(");<><${}${()); )");
    do_str_lit_test("Ææ");
}
