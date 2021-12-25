#[derive(Debug, Clone, Copy)]
pub struct Point {
  offset: usize,
  line: usize,
  column: usize,
}

impl Point {
  pub fn new() -> Point {
    Point {
      offset: 0,
      line: 1,
      column: 1,
    }
  }

  pub fn next(&self, ch: char) -> Point {
    let is_newline = ch == '\n';
    let offset = self.offset + 1;
    let line = if is_newline { self.line + 1 } else { self.line };
    let column = if is_newline { 1 } else { self.column + 1 };

    Point {
      offset,
      line,
      column,
    }
  }
}

impl std::fmt::Display for Point {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}:{})", self.line, self.column)
  }
}

#[derive(Debug)]
pub struct Span {
  pub start: Point,
  pub end: Point,
}

impl Span {
  pub fn new(start: Point, end: Point) -> Span {
    Span { start, end }
  }

  pub fn to_slice<'a>(&self, s: &'a str) -> &'a str {
    &s[self.start.offset..self.end.offset]
  }
}

impl From<Point> for Span {
  fn from(pt: Point) -> Span {
    Span {
      start: pt,
      end: pt.next(' '),
    }
  }
}

pub struct Source<'a> {
  pub name: &'a str,
  pub contents: &'a str,
}

pub struct Error<'a> {
  pub source: &'a Source<'a>,
  pub span: Span,
  pub message: String,
}

impl<'a> std::fmt::Debug for Error<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Error: {message} at {position} in {filename}",
      message = self.message,
      position = self.span.start,
      filename = self.source.name
    )
  }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
