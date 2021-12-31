#[derive(Clone, Copy)]
pub struct Point<'src> {
  src: &'src Source,
  offset: usize,
  line: usize,
  column: usize,
}

impl<'src> Point<'src> {
  pub fn new(src: &'src Source) -> Point {
    Point {
      src,
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
      src: self.src,
      offset,
      line,
      column,
    }
  }
}

impl<'src> std::fmt::Display for Point<'src> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}:{})", self.line, self.column)
  }
}

#[derive(Copy, Clone)]
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

pub struct Source {
  pub name: String,
  pub contents: String,
}

pub struct Error<'src> {
  pub span: Span<'src>,
  pub message: String,
}

impl<'a> std::fmt::Debug for Error<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Error: {message} at {position} in {filename}",
      message = self.message,
      position = self.span.start,
      filename = self.span.start.src.name
    )
  }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
