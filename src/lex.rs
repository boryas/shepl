use crate::{err, Mode};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{anychar, digit1, satisfy},
    combinator::{consumed, map_res, not, peek, recognize, verify},
    multi::many0,
    sequence::{delimited, tuple},
    AsChar, Compare, CompareResult, IResult, InputIter, InputLength, InputTake, Needed,
};
use std::cell::RefCell;
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq)]
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
    If,
    Then,
    Else,
    IntLit(i128),
    StrLit(String),
    Iden(String),
    Raw(String),
    Word(String),
}

#[derive(Clone, Debug)]
pub struct Toks<'a> {
    pub ts: &'a [Tok],
}

impl<'a> Toks<'a> {
    pub fn new(ts: &'a [Tok]) -> Self {
        Toks { ts: ts }
    }
}

impl<'a> InputLength for Toks<'a> {
    fn input_len(&self) -> usize {
        self.ts.len()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LexPos {
    col: usize,
    ln: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lexeme {
    pub tok: Tok,
    pos: LexPos,
    len: usize,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Lexemes<'a> {
    pub lxs: &'a [Lexeme],
}

impl<'a> Lexemes<'a> {
    pub fn new(lxs: &'a [Lexeme]) -> Self {
        Lexemes { lxs: lxs }
    }
}

// TODO: make Lexemes "naturally" iterable via the slice and use that for the nom specific iter
impl<'a> InputIter for Lexemes<'a> {
    type Item = &'a Lexeme;
    type IterElem = std::slice::Iter<'a, Lexeme>;
    type Iter = std::iter::Enumerate<Self::IterElem>;
    // good god
    fn iter_indices(&self) -> Self::Iter {
        self.lxs.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.lxs.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.lxs.iter().position(|b| predicate(b))
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.lxs.len() >= count {
            return Ok(count);
        }
        Err(Needed::new(count - self.lxs.len()))
    }
}

impl<'a> InputTake for Lexemes<'a> {
    fn take(&self, count: usize) -> Self {
        Lexemes::new(&self.lxs[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (
            Lexemes::new(&self.lxs[count..]),
            Lexemes::new(&self.lxs[..count]),
        )
    }
}

impl<'a> InputLength for Lexemes<'a> {
    fn input_len(&self) -> usize {
        self.lxs.len()
    }
}

impl<'a, 'b> Compare<Toks<'b>> for Lexemes<'a> {
    fn compare(&self, toks: Toks<'b>) -> CompareResult {
        let pos = self
            .iter_elements()
            .zip(toks.ts.iter())
            .position(|(l, t)| l.tok != *t);
        match pos {
            None => {
                if self.lxs.len() >= toks.ts.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
            Some(_) => CompareResult::Error,
        }
    }

    fn compare_no_case(&self, toks: Toks<'b>) -> CompareResult {
        self.compare(toks)
    }
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

fn ctx_push_paren(lx: &Lexeme) {
    LEX_CTX.with(|ctx| {
        (*ctx).borrow_mut().parens.push_back(lx.clone());
    })
}

fn ctx_pop_paren() -> Option<Lexeme> {
    LEX_CTX.with(|ctx| {
        // TODO: error if no paren left
        (*ctx).borrow_mut().parens.pop_back().clone()
    })
}

pub fn ctx_reset() {
    LEX_CTX.with(|ctx| {
        *(*ctx).borrow_mut() = LexCtx::new();
    })
}

fn ctx_pos_forward(off: usize) {
    LEX_CTX.with(|ctx| {
        let col: &mut usize = &mut (*ctx).borrow_mut().pos.col;
        *col += off;
    })
}

fn ctx_pos_new_line() {
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

fn ctx_pos_update(consumed: &str) {
    let mut lns = consumed.lines();
    match lns.next() {
        Some(ln) => ctx_pos_forward(ln.len()),
        None => (),
    }
    for ln in lns {
        ctx_pos_new_line();
        ctx_pos_forward(ln.len());
    }
}

pub fn ctx_pos() -> LexPos {
    let mut pos = LexPos { col: 0, ln: 0 };
    LEX_CTX.with(|ctx| pos = (*ctx).borrow().pos);
    pos
}

fn open_mode(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag("$(")(input)?;
    Ok((input, Tok::OpenModeToggle))
}

fn open_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag("(")(input)?;
    Ok((input, Tok::OpenParen))
}

fn close_paren(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
    let (input, _) = tag(")")(input)?;
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

// TODO check a map or a pattern match...
fn end_of_token(c: char) -> bool {
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

pub mod repl {
    use crate::err;
    use crate::lex::{ctx_pos, ctx_pos_update, iden, keyword, lit, op, sep, Lexeme};
    use nom::{branch::alt, character::complete::multispace0, combinator::consumed, IResult};

    pub fn lex_one(input: &str) -> IResult<&str, Lexeme, err::Err<&str>> {
        let (input, w_str1) = multispace0(input)?;
        ctx_pos_update(w_str1);

        let tok_pos = ctx_pos();
        let (input, (tok_str, tok)) = consumed(alt((keyword, sep, op, iden, lit)))(input)?;
        ctx_pos_update(tok_str);

        let (input, w_str2) = multispace0(input)?;
        ctx_pos_update(w_str2);

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

pub mod shell {
    use crate::err;
    use crate::lex::{ctx_pos, ctx_pos_update, sep, Lexeme, Tok};
    use nom::{
        branch::alt, bytes::complete::take_while1, character::complete::multispace0,
        combinator::consumed, IResult,
    };

    fn word_char(c: char) -> bool {
        (c != ' ') && (c != '\t') && (c != '\n') && (c != '\r') && (c != ')') // TODO: what about legit desire to use parens?!
    }

    fn word(input: &str) -> IResult<&str, Tok, err::Err<&str>> {
        let (input, w) = take_while1(word_char)(input)?;
        Ok((input, Tok::Word(w.to_string())))
    }

    pub fn lex_one(input: &str) -> IResult<&str, Lexeme, err::Err<&str>> {
        let (input, w_str1) = multispace0(input)?;
        ctx_pos_update(w_str1);

        let tok_pos = ctx_pos();
        let (input, (tok_str, tok)) = consumed(alt((sep, word)))(input)?;
        ctx_pos_update(tok_str);

        let (input, w_str2) = multispace0(input)?;
        ctx_pos_update(w_str2);

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

pub fn lex<'a, 'b>(
    input: &'a str,
    mode: &'b mut Mode,
) -> IResult<&'a str, Vec<Lexeme>, err::Err<&'a str>> {
    ctx_reset();
    let mut lxs: Vec<Lexeme> = Vec::new();
    let mut inp = input;
    loop {
        let lex_one_fn = match mode {
            Mode::Shell => crate::lex::shell::lex_one,
            Mode::Repl => crate::lex::repl::lex_one,
        };
        let (i_tmp, (lx_str, lx)) = consumed(lex_one_fn)(inp)?;
        inp = i_tmp;
        if lx_str == "" {
            panic!("did not consuuuume");
        }
        match lx.tok {
            Tok::OpenModeToggle => {
                *mode = match mode {
                    Mode::Shell => Mode::Repl,
                    Mode::Repl => Mode::Shell,
                };
                println!("open mode toggle, switch mode to {:?}", *mode);
                ctx_push_paren(&lx);
            }
            Tok::OpenParen => ctx_push_paren(&lx),
            Tok::CloseParen => {
                let open = ctx_pop_paren();
                match open {
                    Some(open_lx) => {
                        if open_lx.tok == Tok::OpenModeToggle {
                            *mode = match mode {
                                Mode::Shell => Mode::Repl,
                                Mode::Repl => Mode::Shell,
                            };
                            println!("close mode toggle, switch mode to {:?}", *mode);
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
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
fn do_lex_test(inputs: Vec<&str>, expected: Vec<Tok>, mut mode: Mode) {
    for input in inputs.into_iter() {
        match lex(input, &mut mode) {
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

#[cfg(test)]
fn do_repl_lex_test(inputs: Vec<&str>, expected: Vec<Tok>) {
    do_lex_test(inputs, expected, Mode::Repl)
}

#[cfg(test)]
fn do_shell_lex_test(inputs: Vec<&str>, expected: Vec<Tok>) {
    do_lex_test(inputs, expected, Mode::Shell)
}

#[test]
fn test_num() {
    let inputs = vec!["42"];
    let toks = vec![Tok::IntLit(42)];
    do_repl_lex_test(inputs, toks);
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
    do_repl_lex_test(inputs, toks);
}

#[test]
fn test_keyword_vs_iden() {
    let i1 = vec!["if f"];
    let toks1 = vec![Tok::If, Tok::Iden("f".to_string())];
    do_repl_lex_test(i1, toks1);
    let i2 = vec!["iff"];
    let toks2 = vec![Tok::Iden("iff".to_string())];
    do_repl_lex_test(i2, toks2);
    let i3 = vec!["if(f)"];
    let toks3 = vec![
        Tok::If,
        Tok::OpenParen,
        Tok::Iden("f".to_string()),
        Tok::CloseParen,
    ];
    do_repl_lex_test(i3, toks3);
}

#[test]
fn test_assign() {
    let inputs = vec!["x = expr"];
    let toks = vec![
        Tok::Iden("x".to_string()),
        Tok::Op(Op::Assign),
        Tok::Iden("expr".to_string()),
    ];
    do_repl_lex_test(inputs, toks);
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
    // Can't use do_repl_lex_test because of String vs &str
    let mut mode = Mode::Repl;
    for input in inputs.into_iter() {
        match lex(&input, &mut mode) {
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
    do_repl_lex_test(inputs, toks);
}

#[test]
fn test_mode_toggle() {
    let inputs = vec!["$()", "$( )"];
    let toks = vec![Tok::OpenModeToggle, Tok::CloseParen];
    do_repl_lex_test(inputs, toks);
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
    do_repl_lex_test(inputs1, toks1);
    let inputs2 = vec!["($())", "( $( ) )"];
    let toks2 = vec![
        Tok::OpenParen,
        Tok::OpenModeToggle,
        Tok::CloseParen,
        Tok::CloseParen,
    ];
    do_repl_lex_test(inputs2, toks2);
}

#[cfg(test)]
fn do_str_lit_test(s: &str) {
    let strung = format!("\"{}\"", s);
    let input = vec![&strung[..]];
    let tok = vec![Tok::StrLit(s.to_string())];
    do_repl_lex_test(input, tok);
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

#[test]
fn test_simple_shell_cmd() {
    let inputs = vec!["ls -l /foo/bar", "   ls    -l    /foo/bar  "];
    let toks = vec![
        Tok::Word("ls".to_string()),
        Tok::Word("-l".to_string()),
        Tok::Word("/foo/bar".to_string()),
    ];
    do_shell_lex_test(inputs, toks);
}

#[test]
fn test_repl_in_shell() {
    let inputs = vec!["ls $(\"foo\")"];
    let toks = vec![
        Tok::Word("ls".to_string()),
        Tok::OpenModeToggle,
        Tok::StrLit("foo".to_string()),
        Tok::CloseParen,
    ];
    do_shell_lex_test(inputs, toks);
}

#[test]
fn test_shell_in_repl() {
    let inputs = vec!["fs = $(ls)"];
    let toks = vec![
        Tok::Iden("fs".to_string()),
        Tok::Op(Op::Assign),
        Tok::OpenModeToggle,
        Tok::Word("ls".to_string()),
        Tok::CloseParen,
    ];
    do_repl_lex_test(inputs, toks);
}

#[test]
fn test_repl_in_shell_in_repl() {
    let inputs = vec!["fs = $(ls $(d))"];
    let toks = vec![
        Tok::Iden("fs".to_string()),
        Tok::Op(Op::Assign),
        Tok::OpenModeToggle,
        Tok::Word("ls".to_string()),
        Tok::OpenModeToggle,
        Tok::Iden("d".to_string()),
        Tok::CloseParen,
        Tok::CloseParen,
    ];
    do_repl_lex_test(inputs, toks);
}

#[test]
fn test_shell_in_repl_in_shell() {
    let inputs = vec!["echo $(some_fn $(ls dir))"];
    let toks = vec![
        Tok::Word("echo".to_string()),
        Tok::OpenModeToggle,
        Tok::Iden("some_fn".to_string()),
        Tok::OpenModeToggle,
        Tok::Word("ls".to_string()),
        Tok::Word("dir".to_string()),
        Tok::CloseParen,
        Tok::CloseParen,
    ];
    do_shell_lex_test(inputs, toks);
}
