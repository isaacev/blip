use super::super::aux::report::{report, Report};
use super::super::err::{Point, Span};
use super::super::lexer::tokens::Token;

pub enum Expr<'src> {
  Let(Let<'src>),
  Paren(Paren<'src>),
  Unary(Unary<'src>),
  Binary(Binary<'src>),
  Name(Name<'src>),
  Integer(Integer<'src>),
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
    }
  }
}

impl Into<Report> for &Expr<'_> {
  fn into(self) -> Report {
    match self {
      Expr::Let(e) => e.into(),
      Expr::Paren(e) => e.into(),
      Expr::Unary(e) => e.into(),
      Expr::Binary(e) => e.into(),
      Expr::Name(e) => e.into(),
      Expr::Integer(e) => e.into(),
    }
  }
}

pub struct Let<'src> {
  pub span: Span<'src>,
  pub name: Name<'src>,
  pub binding: Box<Expr<'src>>,
  pub body: Box<Expr<'src>>,
}

impl Into<Report> for &Let<'_> {
  fn into(self) -> Report {
    report! {
      paren_left
      write("let")
      newline_if_not_blank
      increment_indent
      indent
      paren_left
      write("define")
      space
      then_from(&self.name)
      space
      then_from(&*self.binding)
      paren_right
      newline
      indent
      then_from(&*self.body)
      decrement_indent
      paren_right
    }
  }
}

pub struct Paren<'src> {
  pub span: Span<'src>,
  pub expr: Box<Expr<'src>>,
}

impl Into<Report> for &Paren<'_> {
  fn into(self) -> Report {
    (&*self.expr).into()
  }
}

pub struct Unary<'src> {
  pub operand: Token<'src>,
  pub right: Box<Expr<'src>>,
}

impl Into<Report> for &Unary<'_> {
  fn into(self) -> Report {
    report! {
      paren_left
      write(self.operand.lexeme)
      space
      then_from(&*self.right)
      paren_right
    }
  }
}

pub struct Binary<'src> {
  pub operand: Token<'src>,
  pub left: Box<Expr<'src>>,
  pub right: Box<Expr<'src>>,
}

impl Into<Report> for &Binary<'_> {
  fn into(self) -> Report {
    report! {
      paren_left
      write(self.operand.lexeme)
      space
      then_from(&*self.left)
      space
      then_from(&*self.right)
      paren_right
    }
  }
}

pub struct Name<'src>(pub Token<'src>);

impl Into<Report> for &Name<'_> {
  fn into(self) -> Report {
    report!(write(self.0.lexeme))
  }
}

pub struct Integer<'src>(pub Token<'src>);

impl Into<Report> for &Integer<'_> {
  fn into(self) -> Report {
    report! {
      write(self.0.lexeme)
    }
  }
}
