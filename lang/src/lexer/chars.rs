use super::super::err::Point;

pub trait AsChar {
  fn as_char(&self) -> char;
}

impl AsChar for char {
  fn as_char(&self) -> char {
    *self
  }
}

impl<'src> AsChar for (Point<'src>, char) {
  fn as_char(&self) -> char {
    self.1
  }
}

pub fn is_whitespace<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), ' ' | '\t' | '\n')
}

pub fn is_start_of_word<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), 'a'..='z' | 'A'..='Z' | '_')
}

pub fn is_middle_of_word<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}

pub fn is_delimiter<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), '(' | ')')
}

pub fn is_operator<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), '=' | '+' | '-' | '*' | '>' | '<')
}

pub fn is_digit<C>(ch: &C) -> bool
where
  C: AsChar,
{
  matches!(ch.as_char(), '0'..='9')
}

pub fn is_dot<C>(ch: &C) -> bool
where
  C: AsChar,
{
  ch.as_char() == '.'
}

pub fn is_any<C>(_ch: &C) -> bool
where
  C: AsChar,
{
  true
}
