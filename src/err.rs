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

pub struct Located<T> {
  pub span: Span,
  pub thing: T,
}

impl<T> Located<T> {
  pub fn new(span: Span, thing: T) -> Located<T> {
    Located { span, thing }
  }
}

impl<T> std::fmt::Debug for Located<T>
where
  T: std::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{: <12} {:?}",
      format!("{}", self.span.start),
      self.thing
    )
  }
}
