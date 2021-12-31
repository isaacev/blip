pub mod token {
  use super::super::err::{AsSpan, Span};

  #[derive(Debug, Copy, Clone, PartialEq, Eq)]
  pub enum Kind {
    Symbol,
    Word,
    Integer,
    Float,
    Error,
  }

  impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      let name = match self {
        Kind::Symbol => "symbol",
        Kind::Word => "word",
        Kind::Integer => "integer",
        Kind::Float => "float",
        Kind::Error => "error",
      };

      write!(f, "{}", name)
    }
  }

  pub struct Token<'src> {
    pub kind: Kind,
    pub span: Span<'src>,
    pub lexeme: &'src str,
  }

  impl<'src> Token<'src> {
    pub fn new(kind: Kind, span: Span<'src>, lexeme: &'src str) -> Token<'src> {
      Token { kind, span, lexeme }
    }
  }

  impl<'src> AsSpan for Token<'src> {
    fn as_span(&self) -> Span {
      self.span
    }
  }
}

pub mod ast {
  use super::super::err::{Point, Span};
  use super::super::print::{Document, Printable};
  use super::token::Token;

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
    pub fn start(&self) -> Point<'src> {
      match self {
        Expr::Let(e) => e.span.start,
        Expr::Paren(e) => e.span.start,
        Expr::Unary(e) => e.operand.span.start,
        Expr::Binary(e) => e.left.start(),
        Expr::Name(e) => e.0.span.start,
        Expr::Integer(e) => e.0.span.start,
        Expr::Float(e) => e.0.span.start,
      }
    }

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

  impl<'src> Printable<'src> for Expr<'src> {
    fn to_doc(&self) -> Document<'src> {
      match self {
        Expr::Let(let_) => Document::new()
          .write("(let")
          .line_break()
          .inc()
          .indent()
          .write("(define ")
          .then(let_.name.to_doc())
          .space()
          .then(let_.binding.to_doc())
          .write(")")
          .line_break()
          .indent()
          .then(let_.body.to_doc())
          .dec()
          .write(")"),
        Expr::Paren(paren) => paren.expr.to_doc(),
        Expr::Unary(unary) => Document::new()
          .write("(")
          .write(unary.operand.lexeme)
          .space()
          .then(unary.right.to_doc())
          .write(")"),
        Expr::Binary(binary) => Document::new()
          .write("(")
          .write(binary.operand.lexeme)
          .space()
          .then(binary.left.to_doc())
          .space()
          .then(binary.right.to_doc())
          .write(")"),
        Expr::Name(name) => name.to_doc(),
        Expr::Integer(integer) => Document::new().write(integer.0.lexeme),
        Expr::Float(float) => Document::new().write(float.0.lexeme),
      }
    }
  }

  impl<'src> Printable<'src> for Name<'src> {
    fn to_doc(&self) -> Document<'src> {
      Document::new().write(self.0.lexeme)
    }
  }
}

pub mod ir {
  use super::super::ty;
  use std::cell::Cell;
  use std::fmt;

  pub struct Names {
    next_id: Cell<usize>,
  }

  impl Names {
    pub fn new() -> Self {
      Self {
        next_id: Cell::new(0),
      }
    }

    pub fn next<S: ToString>(&self, canonical: S, ty: ty::Type) -> Name {
      let id = self.next_id.get();
      self.next_id.set(id + 1);
      Name {
        id,
        canonical: canonical.to_string(),
        ty,
      }
    }
  }

  pub struct Name {
    id: usize,
    pub canonical: String,
    pub ty: ty::Type,
  }

  impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{}", self.canonical)
    }
  }

  impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{} :: {}", self.canonical, self.ty)
    }
  }

  impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
      self.id == other.id
    }
  }

  impl Eq for Name {
    // empty
  }

  #[derive(Debug)]
  pub struct Let {
    pub name: Name,
    pub binding: Box<Expr>,
    pub body: Box<Expr>,
  }

  #[derive(Debug)]
  pub struct Binary {
    pub operand: Name,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
  }

  #[derive(Debug)]
  pub struct Unary {
    pub operand: Name,
    pub right: Box<Expr>,
  }

  #[derive(Debug)]
  pub struct Integer {
    pub repr: String,
  }

  #[derive(Debug)]
  pub struct Float {
    pub repr: String,
  }

  #[derive(Debug)]
  pub enum Expr {
    Let(Let),
    Binary(Binary),
    Unary(Unary),
    Name(Name),
    Integer(Integer),
    Float(Float),
  }
}
