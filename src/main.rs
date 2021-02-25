extern crate nom;
use nom::{
  IResult,
  character::complete::alphanumeric1,
};

/* TODO: decide if you want to "do it live" while the input is still valid, or
 * if your String should be copied.
 */
#[derive(Debug,PartialEq)]
pub enum Expr {
  Int(i32),
  Iden(String)
}

fn identifier(input: &str) -> IResult<&str, Expr> {
  let (input, id) = alphanumeric1(input)?;

  Ok((input, Expr::Iden(id.to_string())))
}

fn main() {}

#[test]
fn parse_id() {
  assert_eq!(identifier("foo"), Ok(("", Expr::Iden("foo".to_string()))));
}
