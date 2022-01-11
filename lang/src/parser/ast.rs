use super::super::doc;
use super::super::doc::ToDoc;
use super::super::err::{Point, Span};
use super::super::lexer::tokens::Token;
use std::fmt;

pub enum Expr<'src> {
  Let(Let<'src>),
  Paren(Paren<'src>),
  Unary(Unary<'src>),
  Binary(Binary<'src>),
  Name(Name<'src>),
  Integer(Integer<'src>),
  Float(Float<'src>),
}

impl<'src> Expr<'src> {
  pub fn end(&self) -> Point<'src> {
    match self {
      Expr::Let(e) => e.span.end,
      Expr::Paren(e) => e.span.end,
      Expr::Unary(e) => e.right.end(),
      Expr::Binary(e) => e.right.end(),
      Expr::Name(e) => e.0.span.end,
      Expr::Integer(e) => e.0.span.end,
      Expr::Float(e) => e.0.span.end,
    }
  }
}

impl fmt::Display for Expr<'_> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let s: String = self.to_doc().into();
    write!(f, "{}", s)
  }
}

pub struct Let<'src> {
  pub span: Span<'src>,
  pub name: Name<'src>,
  pub binding: Box<Expr<'src>>,
  pub body: Box<Expr<'src>>,
}

pub struct Paren<'src> {
  pub span: Span<'src>,
  pub expr: Box<Expr<'src>>,
}

pub struct Unary<'src> {
  pub operand: Token<'src>,
  pub right: Box<Expr<'src>>,
}

pub struct Binary<'src> {
  pub operand: Token<'src>,
  pub left: Box<Expr<'src>>,
  pub right: Box<Expr<'src>>,
}

pub struct Name<'src>(pub Token<'src>);

pub struct Integer<'src>(pub Token<'src>);

pub struct Float<'src>(pub Token<'src>);

impl doc::ToDoc for Expr<'_> {
  fn to_doc(&self) -> doc::Doc {
    match self {
      Expr::Let(let_) => {
        let mut d = doc::Doc::new();
        d.write("(let");
        d.newline_if_not_blank();
        d.increment_indent();
        d.indent();
        d.write("(define ");
        d.then(&let_.name);
        d.space();
        d.then(&*let_.binding);
        d.write(")");
        d.newline_if_not_blank();
        d.indent();
        d.then(&*let_.body);
        d.decrement_indent();
        d.write(")");
        d
      }
      Expr::Paren(paren) => (&*paren.expr).to_doc(),
      Expr::Unary(unary) => {
        let mut d = doc::Doc::new();
        d.write("(");
        d.write(unary.operand.lexeme);
        d.space();
        d.then(&*unary.right);
        d.write(")");
        d
      }
      Expr::Binary(binary) => {
        let mut d = doc::Doc::new();
        d.write("(");
        d.write(binary.operand.lexeme);
        d.space();
        d.then(&*binary.left);
        d.space();
        d.then(&*binary.right);
        d.write(")");
        d
      }
      Expr::Name(name) => name.to_doc(),
      Expr::Integer(integer) => {
        let mut d = doc::Doc::new();
        d.write(integer.0.lexeme);
        d
      }
      Expr::Float(float) => {
        let mut d = doc::Doc::new();
        d.write(float.0.lexeme);
        d
      }
    }
  }
}

impl doc::ToDoc for Name<'_> {
  fn to_doc(&self) -> doc::Doc {
    let mut d = doc::Doc::new();
    d.write(self.0.lexeme);
    d
  }
}
