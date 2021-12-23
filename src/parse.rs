use super::err::{Located, Point, Span};
use std::iter::Peekable;
use std::str::Chars;

mod chars {
  use super::*;

  pub trait AsChar {
    fn as_char(&self) -> char;
  }

  impl AsChar for char {
    fn as_char(&self) -> char {
      *self
    }
  }

  impl AsChar for (Point, char) {
    fn as_char(&self) -> char {
      self.1
    }
  }

  pub fn is_whitespace<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    match ch.as_char() {
      ' ' | '\t' | '\n' => true,
      _ => false,
    }
  }

  pub fn is_start_of_word<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    match ch.as_char() {
      'a'..='z' | 'A'..='Z' | '_' => true,
      _ => false,
    }
  }

  pub fn is_middle_of_word<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    match ch.as_char() {
      'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
      _ => false,
    }
  }

  pub fn is_symbol<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    match ch.as_char() {
      '=' | '+' | '-' | '*' => true,
      _ => false,
    }
  }

  pub fn is_digit<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    match ch.as_char() {
      '0'..='9' => true,
      _ => false,
    }
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
}

mod lexer {
  use super::*;

  struct Scanner<'a> {
    chars: Chars<'a>,
    next_point: Point,
  }

  impl Scanner<'_> {
    pub fn new<'a>(file: &'a str) -> Scanner<'a> {
      Scanner {
        chars: file.chars(),
        next_point: Point::new(),
      }
    }
  }

  impl<'a> Iterator for Scanner<'a> {
    type Item = (Point, char);

    fn next(&mut self) -> Option<Self::Item> {
      if let Some(ch) = self.chars.next() {
        let tuple = (self.next_point.clone(), ch);
        self.next_point = self.next_point.next(ch);
        Some(tuple)
      } else {
        None
      }
    }
  }

  #[derive(Debug)]
  pub enum Token<'a> {
    Symbol(&'a str),
    Word(&'a str),
    Numeric(&'a str),
    Error(&'a str),
  }

  type CharGuard<C> = fn(&C) -> bool;
  type ToToken<'a> = fn(&'a str) -> Token<'a>;

  struct Accumulator<'a> {
    to_token: ToToken<'a>,
    start: Point,
    end: Point,
  }

  impl<'a> Accumulator<'a> {
    fn new(to_token: ToToken<'a>, pt: Point, ch: char) -> Self {
      Accumulator {
        to_token,
        start: pt,
        end: pt.next(ch),
      }
    }
  }

  pub struct Lexer<'a> {
    file: &'a str,
    scanner: Peekable<Scanner<'a>>,
    acc: Option<Accumulator<'a>>,
  }

  impl<'a> Lexer<'a> {
    pub fn new(file: &'a str) -> Self {
      Lexer {
        file,
        scanner: Scanner::new(file).peekable(),
        acc: None,
      }
    }

    fn skip_while(&mut self, guard: CharGuard<(Point, char)>) {
      while let Some(_) = self.scanner.next_if(guard) {}
    }

    fn push_if(&mut self, to_token: ToToken<'a>, guard: CharGuard<(Point, char)>) -> bool {
      if let Some((pt, ch)) = self.scanner.next_if(guard) {
        self.acc = Some(Accumulator::new(to_token, pt, ch));
        true
      } else {
        false
      }
    }

    fn pop(&mut self) -> Located<Token<'a>> {
      let acc = self.acc.take().unwrap();
      let span = Span::new(acc.start, acc.end);
      let slice = span.to_slice(self.file);
      let token = Located::new(span, (acc.to_token)(slice));
      token
    }

    fn next_if(&mut self, guard: CharGuard<(Point, char)>) -> bool {
      if let Some(ref mut acc) = self.acc {
        if let Some((pt, ch)) = self.scanner.next_if(guard) {
          acc.end = pt.next(ch);
          return true;
        }
      }

      false
    }

    fn next_all_if(&mut self, guard: CharGuard<(Point, char)>) {
      while self.next_if(guard) {}
    }
  }

  impl<'a> Iterator for Lexer<'a> {
    type Item = Located<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
      self.skip_while(chars::is_whitespace);

      if self.push_if(Token::Symbol, chars::is_symbol) {
        Some(self.pop())
      } else if self.push_if(Token::Word, chars::is_start_of_word) {
        self.next_all_if(chars::is_middle_of_word);
        Some(self.pop())
      } else if self.push_if(Token::Numeric, chars::is_digit) {
        self.next_all_if(chars::is_digit);
        if self.next_if(chars::is_dot) {
          self.next_all_if(chars::is_digit);
        }
        Some(self.pop())
      } else if self.push_if(Token::Error, chars::is_any) {
        Some(self.pop())
      } else {
        None
      }
    }
  }
}

pub fn parse(file: &str) {
  for tok in lexer::Lexer::new(file) {
    println!("{:?}", tok);
  }
}
