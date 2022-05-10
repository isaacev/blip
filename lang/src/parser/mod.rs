pub mod ast;

use super::diag;
use super::err;
use super::err::{Source, Span};
use super::lexer::tokens::{Kind, Token};
use super::lexer::Lexer;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Precedence(u8);

const LOWEST: Precedence = Precedence(0);
const RELATION: Precedence = Precedence(10);
const SUM: Precedence = Precedence(20);
const PRODUCT: Precedence = Precedence(30);
const UNARY: Precedence = Precedence(40);

trait ToPrecedence {
  fn to_prefix_precedence(&self) -> Precedence;
  fn to_infix_precedence(&self) -> Precedence;
}

impl ToPrecedence for Token<'_> {
  fn to_prefix_precedence(&self) -> Precedence {
    match (self.kind, self.lexeme) {
      (Kind::Operator, "+") => UNARY,
      (Kind::Operator, "-") => UNARY,
      (Kind::Word, "print") => UNARY,
      _ => LOWEST,
    }
  }

  fn to_infix_precedence(&self) -> Precedence {
    match (self.kind, self.lexeme) {
      (Kind::Operator, "<") => RELATION,
      (Kind::Operator, "<=") => RELATION,
      (Kind::Operator, ">") => RELATION,
      (Kind::Operator, ">=") => RELATION,
      (Kind::Operator, "+") => SUM,
      (Kind::Operator, "-") => SUM,
      (Kind::Operator, "*") => PRODUCT,
      _ => LOWEST,
    }
  }
}

struct Parser<'src> {
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

  fn next_eof(&mut self) -> diag::Result<()> {
    if let Some(tok) = self.lexer.next() {
      Err(
        diag::ErrorBuilder::from(tok.span)
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
    let right_paren = self.next_token(Kind::Delimiter, ")")?;
    Ok(ast::Paren {
      span: Span::new(left_paren.span.start, right_paren.span.end),
      expr,
    })
  }

  fn let_expr(&mut self, keyword: Token<'src>) -> diag::Result<ast::Let<'src>> {
    let name = self.name()?;
    self.next_token(Kind::Operator, "=")?;
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
    if let Some(oper) = self.next_if_kind(Kind::Operator)? {
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

  fn expr(&mut self, threshold: Precedence) -> diag::Result<ast::Expr<'src>> {
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

pub fn parse<'src>(source: &'src err::Source<'src>) -> diag::Result<ast::Expr<'src>> {
  let mut parser = Parser::from(source);
  let expr = parser.expr(LOWEST)?;
  parser.next_eof()?;
  Ok(expr)
}
