mod chars;
pub mod tokens;

use super::err::{Point, Source, Span};
use std::iter::Peekable;
use std::str::Chars;
use tokens::{Kind, Token};

struct Scanner<'src> {
  chars: Chars<'src>,
  next_point: Point<'src>,
}

impl Scanner<'_> {
  pub fn new<'src>(src: &'src Source) -> Scanner<'src> {
    Scanner {
      chars: src.contents.chars(),
      next_point: Point::new(src),
    }
  }
}

impl<'src> Iterator for Scanner<'src> {
  type Item = (Point<'src>, char);

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(ch) = self.chars.next() {
      let tuple = (self.next_point, ch);
      self.next_point = self.next_point.next(ch);
      Some(tuple)
    } else {
      None
    }
  }
}

type CharGuard<C> = fn(&C) -> bool;

struct Accumulator<'src> {
  kind: Kind,
  start: Point<'src>,
  end: Point<'src>,
}

impl<'src> Accumulator<'src> {
  fn new(kind: Kind, pt: Point<'src>, ch: char) -> Self {
    Accumulator {
      kind,
      start: pt,
      end: pt.next(ch),
    }
  }
}

pub struct Lexer<'src> {
  source: &'src Source<'src>,
  scanner: Peekable<Scanner<'src>>,
  acc: Option<Accumulator<'src>>,
  lookahead: Option<Option<Token<'src>>>,
  pub last_read_point: Point<'src>,
}

impl<'src> Lexer<'src> {
  pub fn new(source: &'src Source) -> Self {
    let scanner = Scanner::new(source);
    let last_read_point = scanner.next_point;
    Lexer {
      source,
      scanner: scanner.peekable(),
      acc: None,
      lookahead: None,
      last_read_point,
    }
  }

  fn skip_while(&mut self, guard: CharGuard<(Point<'src>, char)>) {
    while self.scanner.next_if(guard).is_some() {}
  }

  fn push_if(&mut self, kind: Kind, guard: CharGuard<(Point<'src>, char)>) -> bool {
    if let Some((pt, ch)) = self.scanner.next_if(guard) {
      self.acc = Some(Accumulator::new(kind, pt, ch));
      true
    } else {
      false
    }
  }

  fn pop(&mut self) -> Token<'src> {
    let acc = self.acc.take().unwrap();
    let span = Span::new(acc.start, acc.end);
    let lexeme = span.to_slice(self.source.contents);

    Token::new(acc.kind, span, lexeme)
  }

  fn swap_kind(&mut self, kind: Kind) {
    self.acc.as_mut().unwrap().kind = kind;
  }

  fn next_if(&mut self, guard: CharGuard<(Point<'src>, char)>) -> bool {
    if let Some(ref mut acc) = self.acc {
      if let Some((pt, ch)) = self.scanner.next_if(guard) {
        acc.end = pt.next(ch);
        return true;
      }
    }

    false
  }

  fn next_all_if(&mut self, guard: CharGuard<(Point<'src>, char)>) {
    while self.next_if(guard) {}
  }

  fn read(&mut self) -> Option<Token<'src>> {
    self.skip_while(chars::is_whitespace);

    if self.push_if(Kind::Delimiter, chars::is_delimiter) {
      Some(self.pop())
    } else if self.push_if(Kind::Operator, chars::is_operator) {
      self.next_all_if(chars::is_operator);
      Some(self.pop())
    } else if self.push_if(Kind::Word, chars::is_start_of_word) {
      self.next_all_if(chars::is_middle_of_word);
      Some(self.pop())
    } else if self.push_if(Kind::Integer, chars::is_digit) {
      self.next_all_if(chars::is_digit);

      if self.next_if(chars::is_dot) {
        self.swap_kind(Kind::Float);
        self.next_all_if(chars::is_digit);
      }

      Some(self.pop())
    } else if self.push_if(Kind::Error, chars::is_any) {
      Some(self.pop())
    } else {
      None
    }
  }

  pub fn peek(&mut self) -> Option<&Token<'src>> {
    if self.lookahead.is_none() {
      self.lookahead = Some(self.read());
    }

    self.lookahead.as_ref().unwrap().as_ref()
  }
}

impl<'src> Iterator for Lexer<'src> {
  type Item = Token<'src>;

  fn next(&mut self) -> Option<Self::Item> {
    let next = match self.lookahead {
      Some(_) => self.lookahead.take().unwrap(),
      None => self.read(),
    };

    if let Some(Token { span, .. }) = &next {
      self.last_read_point = span.end;
    }

    next
  }
}
