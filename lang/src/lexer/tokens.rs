use super::super::err::{AsSpan, Span};
use serde::Serialize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize)]
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
