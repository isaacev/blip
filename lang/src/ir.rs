use super::ty;
use std::fmt;

pub struct Name {
  pub ty: ty::Ty,
  pub canonical: String,
}

impl fmt::Display for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.canonical)
  }
}

impl fmt::Debug for Name {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} :: {:?}", self.canonical, self.ty)
  }
}

#[derive(Debug)]
pub struct Let {
  pub name: Name,
  pub binding: Box<Expr>,
  pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Binary {
  pub ty: ty::Ty,
  pub operand: Name,
  pub left: Box<Expr>,
  pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct Unary {
  pub ty: ty::Ty,
  pub operand: Name,
  pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct Integer {
  pub ty: ty::Ty,
  pub repr: String,
}

#[derive(Debug)]
pub struct Float {
  pub ty: ty::Ty,
  pub repr: String,
}

#[derive(Debug)]
pub enum Expr {
  Let(Let),
  Binary(Binary),
  Unary(Unary),
  Name(Name),
  Integer(Integer),
  Float(Float),
}

impl ty::AsTy for Expr {
  fn as_ty(&self) -> &ty::Ty {
    match self {
      Expr::Let(e) => e.body.as_ty(),
      Expr::Binary(e) => &e.ty,
      Expr::Unary(e) => &e.ty,
      Expr::Name(e) => &e.ty,
      Expr::Integer(e) => &e.ty,
      Expr::Float(e) => &e.ty,
    }
  }
}
