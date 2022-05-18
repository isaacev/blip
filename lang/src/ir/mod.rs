use std::collections::HashMap;

use super::aux::report::{report, Report};

pub mod lower;

#[derive(Default)]
pub struct Root {
  pub func: Func,
}

impl From<&Root> for Report {
  fn from(_root: &Root) -> Self {
    report! {
      paren_left
      increment_indent
      write("root")
      newline
      indent
      write("...")
      decrement_indent
      paren_right
    }
  }
}

#[derive(Default)]
pub struct Func {
  block: Block,
}

#[derive(Default)]
pub struct Block {
  stmts: Vec<Stmt>,
}

pub enum Stmt {
  Push(Expr),
  Ret,
}

pub enum Expr {
  Integer(i64),
}
