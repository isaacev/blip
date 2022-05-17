use super::super::aux::report::{report, Report};
use super::super::err::{Point, Span};
use super::super::lexer::tokens::Token;

pub struct Prog<'src> {
  pub items: Vec<Item<'src>>,
}

impl From<&Prog<'_>> for Report {
  fn from(prog: &Prog<'_>) -> Self {
    report! {
      for_each(prog.items.iter(), |item| report! {
        then_from(item)
        newline
      })
    }
  }
}

pub enum Item<'src> {
  Use(Use<'src>),
  Defun(Defun<'src>),
}

impl From<&Item<'_>> for Report {
  fn from(item: &Item<'_>) -> Self {
    match item {
      Item::Use(i) => i.into(),
      Item::Defun(i) => i.into(),
    }
  }
}

pub struct Use<'src> {
  pub name: Name<'src>,
  pub note: Note<'src>,
}

impl From<&Use<'_>> for Report {
  fn from(item: &Use<'_>) -> Self {
    report! {
      paren_left
      write("use")
      space
      then_from(&item.name)
      space
      then_from(&item.note)
      paren_right
    }
  }
}

pub struct Defun<'src> {
  pub name: Name<'src>,
  pub params: Vec<Name<'src>>,
  pub body: Expr<'src>,
}

impl From<&Defun<'_>> for Report {
  fn from(item: &Defun<'_>) -> Self {
    report! {
      paren_left
      write("def")
      space
      then_from(&item.name)
      space
      paren_left
      then_from_all_inline(item.params.iter())
      paren_right
      newline
      increment_indent
      indent
      then_from(&item.body)
      decrement_indent
      paren_right
    }
  }
}

pub enum Note<'src> {
  Const(Name<'src>),
  Arrow(Vec<Note<'src>>, Box<Note<'src>>),
}

impl From<&Note<'_>> for Report {
  fn from(note: &Note<'_>) -> Self {
    match &note {
      Note::Const(name) => report!(then_from(name)),
      Note::Arrow(params, ret) => report! {
        paren_left
        paren_left
        then_from_all_inline(params.iter())
        paren_right
        space
        then_from(&**ret)
        paren_right
      },
    }
  }
}

pub enum Expr<'src> {
  Let(Let<'src>),
  Print(Print<'src>),
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
      Expr::Print(e) => e.span.end,
      Expr::Paren(e) => e.span.end,
      Expr::Unary(e) => e.right.end(),
      Expr::Binary(e) => e.right.end(),
      Expr::Name(e) => e.0.span.end,
      Expr::Integer(e) => e.0.span.end,
    }
  }

  pub fn span<'a>(&'a self) -> Span<'src> {
    match self {
      Expr::Let(e) => e.span,
      Expr::Print(e) => e.span,
      Expr::Paren(e) => e.span,
      Expr::Unary(e) => e.right.span(),
      Expr::Binary(e) => e.right.span(),
      Expr::Name(e) => e.0.span,
      Expr::Integer(e) => e.0.span,
    }
  }
}

impl From<&Expr<'_>> for Report {
  fn from(expr: &Expr<'_>) -> Self {
    match expr {
      Expr::Let(e) => e.into(),
      Expr::Print(e) => e.into(),
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

impl From<&Let<'_>> for Report {
  fn from(expr: &Let<'_>) -> Self {
    report! {
      paren_left
      write("let")
      newline_if_not_blank
      increment_indent
      indent
      paren_left
      write("define")
      space
      then_from(&expr.name)
      space
      then_from(&*expr.binding)
      paren_right
      newline
      indent
      then_from(&*expr.body)
      decrement_indent
      paren_right
    }
  }
}

pub struct Print<'src> {
  pub span: Span<'src>,
  pub arg: Box<Expr<'src>>,
}

impl<'src> From<&Print<'src>> for Report {
  fn from(expr: &Print<'src>) -> Self {
    report! {
      paren_left
      write("print")
      space
      then_from(&*expr.arg)
      paren_right
    }
  }
}

pub struct Paren<'src> {
  pub span: Span<'src>,
  pub expr: Box<Expr<'src>>,
}

impl From<&Paren<'_>> for Report {
  fn from(expr: &Paren<'_>) -> Self {
    (&*expr.expr).into()
  }
}

pub struct Unary<'src> {
  pub operand: Token<'src>,
  pub right: Box<Expr<'src>>,
}

impl From<&Unary<'_>> for Report {
  fn from(expr: &Unary<'_>) -> Self {
    report! {
      paren_left
      write(expr.operand.lexeme)
      space
      then_from(&*expr.right)
      paren_right
    }
  }
}

pub struct Binary<'src> {
  pub operand: Token<'src>,
  pub left: Box<Expr<'src>>,
  pub right: Box<Expr<'src>>,
}

impl From<&Binary<'_>> for Report {
  fn from(expr: &Binary<'_>) -> Self {
    report! {
      paren_left
      write(expr.operand.lexeme)
      space
      then_from(&*expr.left)
      space
      then_from(&*expr.right)
      paren_right
    }
  }
}

pub struct Name<'src>(pub Token<'src>);

impl From<&Name<'_>> for Report {
  fn from(expr: &Name<'_>) -> Self {
    report!(write(expr.0.lexeme))
  }
}

pub struct Integer<'src>(pub Token<'src>);

impl From<&Integer<'_>> for Report {
  fn from(expr: &Integer<'_>) -> Self {
    report!(write(expr.0.lexeme))
  }
}
