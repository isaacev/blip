use super::ty;
use std::fmt;
use std::hash::{Hash, Hasher};

pub trait SymbolId {
  fn id(&self) -> usize;
}

#[derive(Debug, Clone, Eq)]
pub struct NamespaceSymbol {
  pub id: usize,
  pub name: String,
  pub parent: Option<Box<NamespaceSymbol>>,
}

impl fmt::Display for NamespaceSymbol {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(parent) = &self.parent {
      write!(f, "{}::{}", parent, self.name)
    } else {
      write!(f, "::{}", self.name)
    }
  }
}

impl SymbolId for NamespaceSymbol {
  fn id(&self) -> usize {
    self.id
  }
}

impl Hash for NamespaceSymbol {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.id.hash(state);
  }
}

impl PartialEq for NamespaceSymbol {
  fn eq(&self, other: &Self) -> bool {
    self.id == other.id
  }
}

#[derive(Debug, Clone)]
pub struct TypeSymbol {
  pub id: usize,
  pub parent: Option<Box<NamespaceSymbol>>,
}

impl SymbolId for TypeSymbol {
  fn id(&self) -> usize {
    self.id
  }
}

impl Hash for TypeSymbol {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.id.hash(state);
  }
}

impl PartialEq for TypeSymbol {
  fn eq(&self, other: &Self) -> bool {
    self.id == other.id
  }
}

#[derive(Debug, Clone)]
pub enum TermSymbol {
  Item {
    id: usize,
    name: String,
    parent: NamespaceSymbol,
  },
  Local {
    id: usize,
    name: String,
  },
}
impl fmt::Display for TermSymbol {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Item { name, parent, .. } => write!(f, "{}::{}", parent, name),
      Self::Local { name, .. } => write!(f, "{}", name),
    }
  }
}

impl SymbolId for TermSymbol {
  fn id(&self) -> usize {
    match self {
      Self::Item { id, .. } | Self::Local { id, .. } => *id,
    }
  }
}

impl Hash for TermSymbol {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.id().hash(state);
  }
}

impl PartialEq for TermSymbol {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Item { id, .. }, Self::Item { id: other_id, .. }) => id == other_id,
      (Self::Local { id, .. }, Self::Local { id: other_id, .. }) => id == other_id,
      _ => false,
    }
  }
}

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
pub struct Prog {
  pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
  Defun(Defun),
}

#[derive(Debug)]
pub struct Defun {
  pub name: Name,
  pub params: Vec<Name>,
  pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Let {
  pub name: Name,
  pub binding: Box<Expr>,
  pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Print {
  pub arg: Box<Expr>,
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
pub enum Expr {
  Let(Let),
  Print(Print),
  Binary(Binary),
  Unary(Unary),
  Name(Name),
  Integer(Integer),
}

impl ty::AsTy for Expr {
  fn as_ty(&self) -> &ty::Ty {
    match self {
      Expr::Let(e) => e.body.as_ty(),
      Expr::Print(e) => e.arg.as_ty(),
      Expr::Binary(e) => &e.ty,
      Expr::Unary(e) => &e.ty,
      Expr::Name(e) => &e.ty,
      Expr::Integer(e) => &e.ty,
    }
  }
}
