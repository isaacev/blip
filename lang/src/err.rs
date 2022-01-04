use serde::Serialize;

#[derive(Clone, Copy, Serialize)]
pub struct Point<'src> {
  #[serde(skip_serializing)]
  pub source: &'src Source<'src>,
  offset: usize,
  pub line: usize,
  pub column: usize,
}

impl<'src> Point<'src> {
  pub fn new(source: &'src Source) -> Point<'src> {
    Point {
      source,
      offset: 0,
      line: 1,
      column: 1,
    }
  }

  pub fn next(&self, ch: char) -> Point<'src> {
    let is_newline = ch == '\n';
    let offset = self.offset + 1;
    let line = if is_newline { self.line + 1 } else { self.line };
    let column = if is_newline { 1 } else { self.column + 1 };

    Point {
      source: self.source,
      offset,
      line,
      column,
    }
  }
}

#[derive(Copy, Clone, Serialize)]
pub struct Span<'src> {
  pub start: Point<'src>,
  pub end: Point<'src>,
}

impl<'src> Span<'src> {
  pub fn new(start: Point<'src>, end: Point<'src>) -> Span<'src> {
    Span { start, end }
  }

  pub fn to_slice<'a>(&self, s: &'a str) -> &'a str {
    &s[self.start.offset..self.end.offset]
  }
}

impl<'src> From<Point<'src>> for Span<'src> {
  fn from(pt: Point) -> Span {
    Span {
      start: pt,
      end: pt.next(' '),
    }
  }
}

pub trait AsSpan {
  fn as_span(&self) -> Span;
}

#[derive(Serialize)]
pub struct Source<'src> {
  pub filename: &'src str,
  pub contents: &'src str,
}
