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

  pub fn is_symbol<C>(ch: &C) -> bool
  where
    C: AsChar,
  {
    matches!(ch.as_char(), '=' | '+' | '-' | '*' | '>' | '<' | '(' | ')')
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
}

pub mod lexer {
  use super::super::err::{Point, Source, Span};
  use super::super::syntax::token::{Kind, Token};
  use super::chars;
  use std::iter::Peekable;
  use std::str::Chars;

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
}

mod parser {
  use super::super::diag;
  use super::super::err::{Source, Span};
  use super::super::syntax::ast;
  use super::super::syntax::token::{Kind, Token};
  use super::lexer::Lexer;

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
        (Kind::Word, "print") => UNARY,
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

  pub struct Parser<'src> {
    lexer: Lexer<'src>,
  }

  impl<'src> Parser<'src> {
    fn err_wanted_expr_found_token(&self, found: Token<'src>) -> diag::Error {
      diag::ErrorBuilder::from(found.span)
        .title(format!("expected an expression, found {}", found.kind))
        .done()
    }

    fn err_wanted_expr_found_eof(&self) -> diag::Error {
      diag::ErrorBuilder::from(Span::from(self.lexer.last_read_point))
        .title("expected an expression, found end-of-file")
        .done()
    }

    fn err_wanted_token_found_token(&self, expected: Kind, found: Token<'src>) -> diag::Error {
      diag::ErrorBuilder::from(found.span)
        .title(format!("expected {}, found {}", expected, found.kind))
        .done()
    }

    fn err_wanted_token_found_eof(&self, expected: Kind) -> diag::Error {
      diag::ErrorBuilder::from(Span::from(self.lexer.last_read_point))
        .title(format!("expected {}, found end-of-file", expected))
        .done()
    }

    fn err_wanted_oper_found_token(&self, found: Token<'src>) -> diag::Error {
      diag::ErrorBuilder::from(found.span)
        .title(format!("expected an operator, found {}", found.kind))
        .done()
    }

    fn err_wanted_oper_found_eof(&self) -> diag::Error {
      diag::ErrorBuilder::from(Span::from(self.lexer.last_read_point))
        .title("expected an operator, found end-of-file")
        .done()
    }

    fn err_unexpected_char(&self, token: Token<'src>) -> diag::Error {
      diag::ErrorBuilder::from(token.span)
        .title(format!("unexpected character: '{}'", token.lexeme))
        .done()
    }

    fn next(&mut self) -> diag::Result<Option<Token<'src>>> {
      match self.lexer.next() {
        Some(tok) if tok.kind == Kind::Error => Err(self.err_unexpected_char(tok)),
        Some(tok) => Ok(Some(tok)),
        None => Ok(None),
      }
    }

    fn next_kind(&mut self, expect: Kind) -> diag::Result<Token<'src>> {
      match self.next()? {
        Some(tok) if tok.kind == expect => Ok(tok),
        Some(tok) => Err(self.err_wanted_token_found_token(expect, tok)),
        None => Err(self.err_wanted_token_found_eof(expect)),
      }
    }

    fn next_token(&mut self, kind: Kind, lexeme: &str) -> diag::Result<Token<'src>> {
      match self.next()? {
        Some(tok) if tok.kind == kind && tok.lexeme == lexeme => Ok(tok),
        Some(tok) => Err(self.err_wanted_token_found_token(kind, tok)),
        None => Err(self.err_wanted_token_found_eof(kind)),
      }
    }

    fn next_if_kind(&mut self, kind: Kind) -> diag::Result<Option<Token<'src>>> {
      match self.lexer.peek() {
        Some(tok) if tok.kind == kind => Ok(Some((self.next()?).unwrap())),
        _ => Ok(None),
      }
    }

    fn next_if_lexeme(&mut self, keyword: &str) -> diag::Result<Option<Token<'src>>> {
      match self.lexer.peek() {
        Some(tok) if tok.lexeme == keyword => Ok(Some(self.next()?.unwrap())),
        _ => Ok(None),
      }
    }

    pub fn next_eof(&mut self) -> diag::Result<()> {
      if let Some(tok) = self.lexer.next() {
        Err(
          diag::ErrorBuilder::from(Span::from(tok.span))
            .title(format!("expected the end-of-file, found {}", tok.kind))
            .done(),
        )
      } else {
        Ok(())
      }
    }

    fn name(&mut self) -> diag::Result<ast::Name<'src>> {
      let name = self.next_kind(Kind::Word)?;
      Ok(ast::Name(name))
    }

    fn paren_expr(&mut self, left_paren: Token<'src>) -> diag::Result<ast::Paren<'src>> {
      let expr = Box::new(self.expr(LOWEST)?);
      let right_paren = self.next_token(Kind::Symbol, ")")?;
      Ok(ast::Paren {
        span: Span::new(left_paren.span.start, right_paren.span.end),
        expr,
      })
    }

    fn let_expr(&mut self, keyword: Token<'src>) -> diag::Result<ast::Let<'src>> {
      let name = self.name()?;
      self.next_token(Kind::Symbol, "=")?;
      let binding = Box::new(self.expr(LOWEST)?);
      self.next_token(Kind::Word, "in")?;
      let body = self.expr(LOWEST)?;
      let span = Span::new(keyword.span.start, body.end());
      let boxed = Box::new(body);
      Ok(ast::Let {
        span,
        name,
        binding,
        body: boxed,
      })
    }

    fn print_expr(&mut self, operand: Token<'src>) -> diag::Result<ast::Unary<'src>> {
      let right = Box::new(self.expr(UNARY)?);
      Ok(ast::Unary { operand, right })
    }

    fn name_expr(&mut self, word: Token<'src>) -> diag::Result<ast::Name<'src>> {
      Ok(ast::Name(word))
    }

    fn integer_expr(&mut self, integer: Token<'src>) -> diag::Result<ast::Integer<'src>> {
      Ok(ast::Integer(integer))
    }

    fn float_expr(&mut self, float: Token<'src>) -> diag::Result<ast::Float<'src>> {
      Ok(ast::Float(float))
    }

    fn prefix_expr(&mut self) -> diag::Result<ast::Expr<'src>> {
      if let Some(left) = self.next_if_lexeme("(")? {
        self.paren_expr(left).map(ast::Expr::Paren)
      } else if let Some(keyword) = self.next_if_lexeme("let")? {
        self.let_expr(keyword).map(ast::Expr::Let)
      } else if let Some(keyword) = self.next_if_lexeme("print")? {
        self.print_expr(keyword).map(ast::Expr::Unary)
      } else if let Some(word) = self.next_if_kind(Kind::Word)? {
        self.name_expr(word).map(ast::Expr::Name)
      } else if let Some(integer) = self.next_if_kind(Kind::Integer)? {
        self.integer_expr(integer).map(ast::Expr::Integer)
      } else if let Some(float) = self.next_if_kind(Kind::Float)? {
        self.float_expr(float).map(ast::Expr::Float)
      } else if let Some(tok) = self.next()? {
        Err(self.err_wanted_expr_found_token(tok))
      } else {
        Err(self.err_wanted_expr_found_eof())
      }
    }

    fn binary_expr(
      &mut self,
      left: ast::Expr<'src>,
      operand: Token<'src>,
    ) -> diag::Result<ast::Binary<'src>> {
      let left = Box::new(left);
      let right = Box::new(self.expr(operand.to_infix_precedence())?);
      Ok(ast::Binary {
        operand,
        left,
        right,
      })
    }

    fn postfix_expr(&mut self, left: ast::Expr<'src>) -> diag::Result<ast::Expr<'src>> {
      if let Some(oper) = self.next_if_kind(Kind::Symbol)? {
        self.binary_expr(left, oper).map(ast::Expr::Binary)
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

    pub fn expr(&mut self, threshold: Precedence) -> diag::Result<ast::Expr<'src>> {
      let mut prefix = self.prefix_expr()?;
      while threshold < self.peek_precedence() {
        prefix = self.postfix_expr(prefix)?;
      }
      Ok(prefix)
    }
  }

  impl<'src> From<&'src Source<'src>> for Parser<'src> {
    fn from(source: &'src Source) -> Parser<'src> {
      Parser {
        lexer: Lexer::new(source),
      }
    }
  }
}

use super::diag;
use super::err;
use super::syntax::ast;

pub fn parse<'src>(source: &'src err::Source<'src>) -> diag::Result<ast::Expr<'src>> {
  let mut parser = parser::Parser::from(source);
  let expr = parser.expr(parser::LOWEST)?;
  parser.next_eof()?;
  Ok(expr)
}
