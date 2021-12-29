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

  #[derive(Debug)]
  pub struct Token<'a> {
    pub kind: Kind,
    pub span: Span,
    pub lexeme: &'a str,
  }

  impl<'a> Token<'a> {
    pub fn new(kind: Kind, span: Span, lexeme: &'a str) -> Token<'a> {
      Token { kind, span, lexeme }
    }
  }

  impl<'a> AsSpan for Token<'a> {
    fn as_span(&self) -> Span {
      self.span
    }
  }
}

pub mod ast {
  use super::super::err::{AsSpan, Span};
  use super::super::print::{Document, Printable};
  use super::token::Token;

  pub enum Expr<'a> {
    Let(Let<'a>),
    Paren(Paren<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
    Name(Name<'a>),
    Integer(Integer<'a>),
    Float(Float<'a>),
  }

  pub struct Let<'a> {
    pub span: Span,
    pub name: Name<'a>,
    pub binding: Box<Expr<'a>>,
    pub body: Box<Expr<'a>>,
  }

  pub struct Paren<'a> {
    pub span: Span,
    pub expr: Box<Expr<'a>>,
  }

  pub struct Unary<'a> {
    pub operand: Token<'a>,
    pub right: Box<Expr<'a>>,
  }

  pub struct Binary<'a> {
    pub operand: Token<'a>,
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
  }

  pub struct Name<'a>(pub Token<'a>);

  pub struct Integer<'a>(pub Token<'a>);

  pub struct Float<'a>(pub Token<'a>);

  impl<'a> AsSpan for Expr<'a> {
    fn as_span(&self) -> Span {
      match self {
        Expr::Let(e) => e.span,
        Expr::Paren(e) => e.span,
        Expr::Unary(e) => e.operand.as_span().unify(&e.right.as_span()),
        Expr::Binary(e) => e.left.as_span().unify(&e.right.as_span()),
        Expr::Name(e) => e.0.as_span(),
        Expr::Integer(e) => e.0.as_span(),
        Expr::Float(e) => e.0.as_span(),
      }
    }
  }

  impl<'a> Printable<'a> for Expr<'a> {
    fn to_doc(&self) -> Document<'a> {
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

  impl<'a> Printable<'a> for Name<'a> {
    fn to_doc(&self) -> Document<'a> {
      Document::new().write(self.0.lexeme)
    }
  }
}
