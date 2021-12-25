mod chars {
  use super::super::err::Point;

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
      '=' | '+' | '-' | '*' | '>' | '<' => true,
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
  use super::super::err::{Point, Source, Span};
  use super::chars;
  use std::iter::Peekable;
  use std::str::Chars;

  struct Scanner<'a> {
    chars: Chars<'a>,
    next_point: Point,
  }

  impl Scanner<'_> {
    pub fn new<'a>(src: &'a Source) -> Scanner<'a> {
      Scanner {
        chars: src.contents.chars(),
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
    fn new(kind: Kind, span: Span, lexeme: &'a str) -> Token<'a> {
      Token { kind, span, lexeme }
    }
  }

  type CharGuard<C> = fn(&C) -> bool;

  struct Accumulator {
    kind: Kind,
    start: Point,
    end: Point,
  }

  impl Accumulator {
    fn new(kind: Kind, pt: Point, ch: char) -> Self {
      Accumulator {
        kind,
        start: pt,
        end: pt.next(ch),
      }
    }
  }

  pub struct Lexer<'a> {
    source: &'a Source,
    scanner: Peekable<Scanner<'a>>,
    acc: Option<Accumulator>,
    lookahead: Option<Option<Token<'a>>>,
    pub last_read_point: Point,
  }

  impl<'a> Lexer<'a> {
    pub fn new(source: &'a Source) -> Self {
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

    fn skip_while(&mut self, guard: CharGuard<(Point, char)>) {
      while let Some(_) = self.scanner.next_if(guard) {}
    }

    fn push_if(&mut self, kind: Kind, guard: CharGuard<(Point, char)>) -> bool {
      if let Some((pt, ch)) = self.scanner.next_if(guard) {
        self.acc = Some(Accumulator::new(kind, pt, ch));
        true
      } else {
        false
      }
    }

    fn pop(&mut self) -> Token<'a> {
      let acc = self.acc.take().unwrap();
      let span = Span::new(acc.start, acc.end);
      let lexeme = span.to_slice(&self.source.contents);
      let token = Token::new(acc.kind, span, lexeme);
      token
    }

    fn swap_kind(&mut self, kind: Kind) {
      self.acc.as_mut().unwrap().kind = kind;
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

    fn read(&mut self) -> Option<Token<'a>> {
      self.skip_while(chars::is_whitespace);

      if self.push_if(Kind::Symbol, chars::is_symbol) {
        self.next_all_if(chars::is_symbol);
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

    pub fn peek(&mut self) -> Option<&Token<'a>> {
      if self.lookahead.is_none() {
        self.lookahead = Some(self.read());
      }

      self.lookahead.as_ref().unwrap().as_ref()
    }
  }

  impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

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
}

mod parser {
  use super::super::ast;
  use super::super::err;
  use super::super::err::{Source, Span};
  use super::lexer::{Kind, Lexer, Token};

  #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
  pub struct Precedence(u8);

  pub const LOWEST: Precedence = Precedence(0);
  pub const RELATION: Precedence = Precedence(10);
  pub const SUM: Precedence = Precedence(20);
  pub const PRODUCT: Precedence = Precedence(30);
  pub const UNARY: Precedence = Precedence(40);

  trait ToPrecedence {
    fn to_prefix_precedence(&self) -> Precedence;
    fn to_infix_precedence(&self) -> Precedence;
  }

  impl ToPrecedence for Token<'_> {
    fn to_prefix_precedence(&self) -> Precedence {
      match (self.kind, self.lexeme) {
        (Kind::Symbol, "+") => UNARY,
        (Kind::Symbol, "-") => UNARY,
        _ => LOWEST,
      }
    }

    fn to_infix_precedence(&self) -> Precedence {
      match (self.kind, self.lexeme) {
        (Kind::Symbol, "<") => RELATION,
        (Kind::Symbol, "<=") => RELATION,
        (Kind::Symbol, ">") => RELATION,
        (Kind::Symbol, ">=") => RELATION,
        (Kind::Symbol, "+") => SUM,
        (Kind::Symbol, "-") => SUM,
        (Kind::Symbol, "*") => PRODUCT,
        _ => LOWEST,
      }
    }
  }

  pub struct Parser<'a> {
    source: &'a Source,
    lexer: Lexer<'a>,
  }

  impl<'a> Parser<'a> {
    fn err_wanted_expr_found_token(&self, found: Token) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: found.span,
        message: format!("expected an expression, found {}", found.kind),
      }
    }

    fn err_wanted_expr_found_eof(&self) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: Span::from(self.lexer.last_read_point),
        message: "expected an expression, found end-of-file".to_owned(),
      }
    }

    fn err_wanted_token_found_token(&self, expected: Kind, found: Token) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: found.span,
        message: format!("expected {}, found {}", expected, found.kind),
      }
    }

    fn err_wanted_token_found_eof(&self, expected: Kind) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: Span::from(self.lexer.last_read_point),
        message: format!("expected {}, found end-of-file", expected),
      }
    }

    fn err_wanted_oper_found_token(&self, found: Token) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: found.span,
        message: format!("expected an operator, found {}", found.kind),
      }
    }

    fn err_wanted_oper_found_eof(&self) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: Span::from(self.lexer.last_read_point),
        message: "expected an operator, found end-of-file".to_owned(),
      }
    }

    fn err_unexpected_char(&self, token: Token<'a>) -> err::Error<'a> {
      err::Error {
        source: self.source,
        span: token.span,
        message: format!("unexpected character: '{}'", token.lexeme),
      }
    }

    fn next(&mut self) -> err::Result<'a, Option<Token<'a>>> {
      match self.lexer.next() {
        Some(tok) if tok.kind == Kind::Error => Err(self.err_unexpected_char(tok)),
        Some(tok) => Ok(Some(tok)),
        None => Ok(None),
      }
    }

    fn next_kind(&mut self, expect: Kind) -> err::Result<'a, &'a str> {
      match self.next()? {
        Some(Token { kind, lexeme, .. }) if kind == expect => Ok(lexeme),
        Some(tok) => Err(self.err_wanted_token_found_token(expect, tok)),
        None => Err(self.err_wanted_token_found_eof(expect)),
      }
    }

    fn next_token(&mut self, ekind: Kind, elexeme: &str) -> err::Result<'a, &'a str> {
      match self.next()? {
        Some(Token { kind, lexeme, .. }) if kind == ekind && lexeme == elexeme => Ok(lexeme),
        Some(tok) => Err(self.err_wanted_token_found_token(ekind, tok)),
        None => Err(self.err_wanted_token_found_eof(ekind)),
      }
    }

    fn next_if_kind(&mut self, kind: Kind) -> err::Result<'a, Option<Token<'a>>> {
      match self.lexer.peek() {
        Some(tok) if tok.kind == kind => Ok(Some((self.next()?).unwrap())),
        _ => Ok(None),
      }
    }

    fn next_if_lexeme(&mut self, keyword: &str) -> err::Result<'a, Option<Token<'a>>> {
      match self.lexer.peek() {
        Some(tok) if tok.lexeme == keyword => Ok(Some(self.next()?.unwrap())),
        _ => Ok(None),
      }
    }

    fn peek_if_not_lexeme(&mut self, keyword: &str) -> bool {
      match self.lexer.peek() {
        Some(tok) if tok.lexeme != keyword => true,
        _ => false,
      }
    }

    fn name(&mut self) -> err::Result<'a, ast::Name<'a>> {
      let name = self.next_kind(Kind::Word)?;
      Ok(ast::Name(name))
    }

    fn binding(&mut self) -> err::Result<'a, ast::Binding<'a>> {
      let name = self.name()?;
      self.next_token(Kind::Symbol, "=")?;
      let expr = self.expr(LOWEST)?;
      Ok(ast::Binding(name, expr))
    }

    fn let_expr(&mut self, _keyword: Token<'a>) -> err::Result<'a, ast::Expr<'a>> {
      let mut bindings = vec![self.binding()?];
      while self.peek_if_not_lexeme("in") {
        bindings.push(self.binding()?);
      }
      self.next_token(Kind::Word, "in")?;
      let body = self.expr(LOWEST)?;
      Ok(ast::Expr::Let(bindings, Box::new(body)))
    }

    fn name_expr(&mut self, word: Token<'a>) -> err::Result<'a, ast::Expr<'a>> {
      let name = ast::Name(word.lexeme);
      Ok(ast::Expr::Name(name))
    }

    fn integer_expr(&mut self, integer: Token<'a>) -> err::Result<'a, ast::Expr<'a>> {
      let value = integer.lexeme.parse::<u64>().unwrap();
      Ok(ast::Expr::Integer(value))
    }

    fn float_expr(&mut self, float: Token<'a>) -> err::Result<'a, ast::Expr<'a>> {
      let value = float.lexeme.parse::<f64>().unwrap();
      Ok(ast::Expr::Float(value))
    }

    fn prefix_expr(&mut self) -> err::Result<'a, ast::Expr<'a>> {
      if let Some(keyword) = self.next_if_lexeme("let")? {
        self.let_expr(keyword)
      } else if let Some(word) = self.next_if_kind(Kind::Word)? {
        self.name_expr(word)
      } else if let Some(integer) = self.next_if_kind(Kind::Integer)? {
        self.integer_expr(integer)
      } else if let Some(float) = self.next_if_kind(Kind::Float)? {
        self.float_expr(float)
      } else if let Some(tok) = self.next()? {
        Err(self.err_wanted_expr_found_token(tok))
      } else {
        Err(self.err_wanted_expr_found_eof())
      }
    }

    fn binary_expr(
      &mut self,
      left: ast::Expr<'a>,
      oper: Token<'a>,
    ) -> err::Result<'a, ast::Expr<'a>> {
      let right = self.expr(oper.to_infix_precedence())?;
      Ok(ast::Expr::Binary(
        Box::new(left),
        oper.lexeme,
        Box::new(right),
      ))
    }

    fn postfix_expr(&mut self, left: ast::Expr<'a>) -> err::Result<'a, ast::Expr<'a>> {
      if let Some(oper) = self.next_if_kind(Kind::Symbol)? {
        self.binary_expr(left, oper)
      } else if let Some(tok) = self.next()? {
        Err(self.err_wanted_oper_found_token(tok))
      } else {
        Err(self.err_wanted_oper_found_eof())
      }
    }

    fn peek_precedence(&mut self) -> Precedence {
      self
        .lexer
        .peek()
        .map(Token::to_infix_precedence)
        .unwrap_or(LOWEST)
    }

    pub fn expr(&mut self, threshold: Precedence) -> err::Result<'a, ast::Expr<'a>> {
      let mut prefix = self.prefix_expr()?;
      while threshold < self.peek_precedence() {
        prefix = self.postfix_expr(prefix)?;
      }
      Ok(prefix)
    }
  }

  impl<'a> From<&'a Source> for Parser<'a> {
    fn from(source: &'a Source) -> Parser<'a> {
      Parser {
        source,
        lexer: Lexer::new(source),
      }
    }
  }
}

use super::ast;
use super::err;

pub fn parse<'a>(source: &'a err::Source) -> err::Result<ast::Expr<'a>> {
  parser::Parser::from(source).expr(parser::LOWEST)
}