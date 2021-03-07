use harsh::interp::repl;

fn main() {
    repl();
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
fn parse_str() {
    assert_eq!(str("\"hello\""), Ok(("", Expr::Str("hello".to_string()))));
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
        _ => assert!(false),
    };
}

#[test]
fn parse_apply() {
    let res = apply("foo 42");
    match res {
        Ok(("", Expr::Apply(f, _e2))) => {
            assert_eq!(f, "foo".to_string());
            //assert_eq!(*e2, Expr::UInt64(42));
            //TODO: test Vec contents
        }
        _ => assert!(false),
    };
}

#[test]
fn parse_cond() {
    let c = expr("if foo then 42 else 43");
    match c {
        Ok(("", Expr::Cond(p, b1, b2))) => {
            assert_eq!(*p, Expr::Iden("foo".to_string()));
            assert_eq!(*b1, Expr::UInt64(42));
            assert_eq!(*b2, Expr::UInt64(43));
        }
        _ => {
            println!("{:?}", c);
            assert!(false)
        }
    }

    assert!(expr("if foo").is_err());
    assert!(expr("if foo then 42").is_err());
    assert!(expr("if then 42 else 42").is_err());
}

#[test]
fn parse_assign() {
    let a = expr("let x = 42");
    match a {
        Ok(("", Expr::Assign(iden, expr))) => {
            assert_eq!(iden, "x".to_string());
            assert_eq!(*expr, Expr::UInt64(42));
        }
        _ => {
            println!("{:?}", a);
            assert!(false)
        }
    }
}
