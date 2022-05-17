use super::diag;
use super::ir;
use super::parser::ast;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

pub type Name = String;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id(usize);

impl fmt::Display for Id {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(usize);

impl Level {
  fn zero() -> Self {
    Self(0)
  }

  fn increment(&self) -> Self {
    Self(self.0 + 1)
  }
}

#[derive(Debug, Clone)]
pub enum Var {
  Unbound(Id, Level),
  Link(Box<Ty>),
  Generic(Id),
}

struct VarNames {
  next: u32,
  cache: HashMap<Id, String>,
}

impl VarNames {
  fn next_for(&mut self, id: Id) -> String {
    let i = self.next;
    self.next += 1;
    let mut name = String::new();
    name.push(char::from_u32(97 + (i % 26)).unwrap());
    if i >= 26 {
      name.push_str(&(i / 26).to_string());
    }
    self.cache.insert(id, name.clone());
    name
  }

  fn lookup(&self, id: &Id) -> Option<String> {
    self.cache.get(id).cloned()
  }
}

#[derive(Debug, Clone)]
pub enum Ty {
  Const(Name),
  Arrow(Vec<Self>, Box<Self>),
  Var(Rc<RefCell<Var>>),
}

pub const INT_NAME: &'static str = "int";
pub const VOID_NAME: &'static str = "void";

impl Ty {
  pub fn int() -> Self {
    Self::Const(INT_NAME.to_owned())
  }

  pub fn void() -> Self {
    Self::Const(VOID_NAME.to_owned())
  }

  pub fn is_const(&self, want: &'static str) -> bool {
    match &self {
      Self::Const(name) if name == want => true,
      Self::Var(cell) => match &*(cell.borrow()) {
        Var::Link(ty) => ty.is_const(want),
        _ => false,
      },
      _ => false,
    }
  }
}

trait WrapWithParens
where
  Self: Sized,
{
  fn wrap_with_parens(self) -> Self;

  fn wrap_with_parens_if(self, condition: bool) -> Self {
    if condition {
      self.wrap_with_parens()
    } else {
      self
    }
  }
}

impl WrapWithParens for String {
  fn wrap_with_parens(self) -> Self {
    format!("({})", self)
  }
}

impl Ty {
  fn have_same_unbound_ids(a: &Ty, b: &Ty) -> bool {
    if let (Ty::Var(ref cell_a), Ty::Var(cell_b)) = (a, b) {
      if let (Var::Unbound(id_a, ..), Var::Unbound(id_b, ..)) =
        (&*cell_a.borrow(), &*cell_b.borrow())
      {
        return id_a == id_b;
      }
    }

    false
  }

  fn ty_to_string(&self, is_simple: bool, names: &mut VarNames) -> String {
    match self {
      Self::Const(name) => name.to_string(),
      Self::Arrow(params, ret) => {
        let params = if params.len() == 1 {
          params.get(0).unwrap().ty_to_string(true, names)
        } else {
          params
            .iter()
            .map(|p| p.ty_to_string(false, names))
            .collect::<Vec<String>>()
            .join(", ")
            .wrap_with_parens()
        };
        let ret = ret.ty_to_string(false, names);
        format!("{} -> {}", params, ret).wrap_with_parens_if(is_simple)
      }
      Self::Var(cell) => match &*(cell.borrow()) {
        Var::Generic(id) => names.lookup(id).unwrap_or_else(|| names.next_for(*id)),
        Var::Unbound(id, ..) => format!("_{}", id),
        Var::Link(ty) => ty.ty_to_string(is_simple, names),
      },
    }
  }
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut names = VarNames {
      next: 0,
      cache: HashMap::new(),
    };
    write!(f, "{}", self.ty_to_string(false, &mut names))
  }
}

struct Ids {
  next: usize,
}

impl Ids {
  fn new() -> Self {
    Self { next: 0 }
  }

  fn next(&mut self) -> Id {
    let next = Id(self.next);
    self.next += 1;
    next
  }

  fn new_var(&mut self, level: Level) -> Ty {
    Ty::Var(Rc::new(RefCell::new(Var::Unbound(self.next(), level))))
  }
}

struct Symbols {
  next: usize,
}

impl Symbols {
  fn _next<T, F: FnOnce(usize) -> T>(&mut self, f: F) -> T {
    let next = f(self.next);
    self.next += 1;
    next
  }

  fn next_root_namespace<S>(&mut self, name: S) -> ir::NamespaceSymbol
  where
    S: ToString,
  {
    self._next(|id| ir::NamespaceSymbol {
      id,
      name: name.to_string(),
      parent: None,
    })
  }

  // fn next_child_namespace<S>(&mut self, name: S, parent: ir::NamespaceSymbol) -> ir::NamespaceSymbol
  // where
  //   S: ToString,
  // {
  //   self._next(|id| ir::NamespaceSymbol {
  //     id,
  //     name: name.to_string(),
  //     parent: Some(Box::new(parent)),
  //   })
  // }

  fn next_item<S>(&mut self, name: S, parent: ir::NamespaceSymbol) -> ir::TermSymbol
  where
    S: ToString,
  {
    self._next(|id| ir::TermSymbol::Item {
      id,
      name: name.to_string(),
      parent,
    })
  }

  fn next_local<S>(&mut self, name: S) -> ir::TermSymbol
  where
    S: ToString,
  {
    self._next(|id| ir::TermSymbol::Local {
      id,
      name: name.to_string(),
    })
  }
}

type TermTyPair = (ir::TermSymbol, Ty);

#[derive(Clone)]
struct Env {
  lexemes: HashMap<String, TermTyPair>,
}

impl Env {
  fn new() -> Self {
    Self {
      lexemes: HashMap::new(),
    }
  }

  fn extend<S: ToString>(&self, lexeme: S, sym: ir::TermSymbol, ty: Ty) -> Env {
    let mut lexemes = self.lexemes.clone();
    lexemes.insert(lexeme.to_string(), (sym, ty));
    Self { lexemes }
  }

  fn insert<S: ToString>(&mut self, lexeme: S, sym: ir::TermSymbol, ty: Ty) {
    self.lexemes.insert(lexeme.to_string(), (sym, ty));
  }

  fn lookup<S: AsRef<str>>(&self, lexeme: S) -> Option<&TermTyPair> {
    self.lexemes.get(lexeme.as_ref())
  }
}

fn occurs_check_adjust_levels(ty: &Ty, id: Id, level: Level) {
  match ty {
    Ty::Var(cell) => {
      let var = cell.borrow().clone();
      match &var {
        Var::Link(ty) => occurs_check_adjust_levels(ty, id, level),
        Var::Generic(..) => unreachable!(),
        Var::Unbound(other_id, other_level) => {
          if *other_id == id {
            panic!("recursive types")
          } else if *other_level > level {
            *cell.borrow_mut() = Var::Unbound(*other_id, level);
          }
        }
      }
    }
    Ty::Arrow(params, ret) => {
      for param in params {
        occurs_check_adjust_levels(param, id, level);
      }
      occurs_check_adjust_levels(ret, id, level);
    }
    Ty::Const(..) => (),
  }
}

struct UnificationError(Ty, Ty);

fn unify(ty_a: &Ty, ty_b: &Ty) -> Result<(), UnificationError> {
  if Ty::have_same_unbound_ids(ty_a, ty_b) {
    unreachable!()
  }

  match (ty_a, ty_b) {
    (Ty::Const(name_a), Ty::Const(name_b)) if name_a == name_b => Ok(()),
    (Ty::Arrow(params_a, ret_a), Ty::Arrow(params_b, ret_b)) => {
      assert_eq!(params_a.len(), params_b.len());
      for (param_a, param_b) in params_a.iter().zip(params_b) {
        unify(param_a, param_b)?;
      }
      unify(ret_a, ret_b)
    }
    (Ty::Var(cell_a), _) => {
      let var_a = cell_a.borrow().clone();
      match &var_a {
        Var::Link(ty_a) => unify(ty_a, ty_b),
        Var::Unbound(id, level) => {
          occurs_check_adjust_levels(ty_b, *id, *level);
          *cell_a.borrow_mut() = Var::Link(Box::new(ty_b.clone()));
          Ok(())
        }
        _ => Err(UnificationError(ty_a.clone(), ty_b.clone())),
      }
    }
    (_, Ty::Var(cell_b)) => {
      let var_b = cell_b.borrow().clone();
      match &var_b {
        Var::Link(ty_b) => unify(ty_a, ty_b),
        Var::Unbound(id, level) => {
          occurs_check_adjust_levels(ty_a, *id, *level);
          *cell_b.borrow_mut() = Var::Link(Box::new(ty_a.clone()));
          Ok(())
        }
        _ => Err(UnificationError(ty_a.clone(), ty_b.clone())),
      }
    }
    _ => Err(UnificationError(ty_a.clone(), ty_b.clone())),
  }
}

fn generalize(ty: &Ty, level: Level) -> Ty {
  match ty {
    Ty::Arrow(params, ret) => Ty::Arrow(
      params.iter().map(|p| generalize(p, level)).collect(),
      Box::new(generalize(ret, level)),
    ),
    Ty::Var(cell) => match &*cell.borrow() {
      Var::Unbound(id, other_level, ..) if *other_level > level => {
        Ty::Var(Rc::new(RefCell::new(Var::Generic(*id))))
      }
      Var::Link(ty) => generalize(ty, level),
      Var::Generic(..) | Var::Unbound(..) => ty.clone(),
    },
    Ty::Const(..) => ty.clone(),
  }
}

fn instantiate(ids: &mut Ids, id_var_map: &mut HashMap<Id, Ty>, ty: &Ty, level: Level) -> Ty {
  match ty {
    Ty::Const(..) => ty.clone(),
    Ty::Arrow(params, ret) => Ty::Arrow(
      params
        .iter()
        .map(|p| instantiate(ids, id_var_map, p, level))
        .collect(),
      Box::new(instantiate(ids, id_var_map, ret, level)),
    ),
    Ty::Var(cell) => match &*cell.borrow() {
      Var::Link(ty) => instantiate(ids, id_var_map, ty, level),
      Var::Generic(id) => {
        if let Some(var) = id_var_map.get(id) {
          var.clone()
        } else {
          let var = ids.new_var(level);
          id_var_map.insert(*id, var.clone());
          var
        }
      }
      Var::Unbound(..) => ty.clone(),
    },
  }
}

enum FuncError {
  NotCallable,
  WrongNumberOfArgs { expected: usize },
}

fn match_fun_ty(num_params: usize, ids: &mut Ids, ty: &Ty) -> Result<(Vec<Ty>, Ty), FuncError> {
  match ty {
    Ty::Arrow(params_ty, ret_ty) => {
      if params_ty.len() != num_params {
        Err(FuncError::WrongNumberOfArgs {
          expected: params_ty.len(),
        })
      } else {
        Ok((params_ty.clone(), *ret_ty.clone()))
      }
    }
    Ty::Var(cell) => {
      let var = cell.borrow().clone();
      match &var {
        Var::Link(ty) => match_fun_ty(num_params, ids, &*ty),
        Var::Unbound(_id, level) => {
          let params_ty: Vec<Ty> = (0..num_params)
            .into_iter()
            .map(|_| ids.new_var(*level))
            .collect();
          let ret_ty = ids.new_var(*level);
          *cell.borrow_mut() = Var::Link(Box::new(Ty::Arrow(
            params_ty.clone(),
            Box::new(ret_ty.clone()),
          )));
          Ok((params_ty, ret_ty))
        }
        _ => Err(FuncError::NotCallable),
      }
    }
    _ => Err(FuncError::NotCallable),
  }
}

pub trait AsTy {
  fn as_ty(&self) -> &Ty;
}

fn lower_expr<'src>(
  expr: &ast::Expr<'src>,
  ids: &mut Ids,
  syms: &mut Symbols,
  env: &mut Env,
  level: Level,
) -> Result<ir::Expr, diag::Error> {
  match expr {
    ast::Expr::Paren(e) => lower_expr(&*e.expr, ids, syms, env, level),
    ast::Expr::Let(e) => {
      let value_expr = lower_expr(&e.binding, ids, syms, env, level.increment())?;
      let gen_ty = generalize(value_expr.as_ty(), level);

      let lexeme = e.name.0.lexeme;
      let sym = syms.next_local(lexeme);
      let env = &mut env.extend(lexeme, sym, gen_ty);

      let body_expr = lower_expr(&e.body, ids, syms, env, level)?;
      Ok(ir::Expr::Let(ir::Let {
        name: ir::Name {
          ty: value_expr.as_ty().clone(),
          canonical: e.name.0.lexeme.into(),
        },
        binding: Box::new(value_expr),
        body: Box::new(body_expr),
      }))
    }
    ast::Expr::Print(e) => {
      let arg = lower_expr(&e.arg, ids, syms, env, level)?;
      if arg.as_ty().is_const(INT_NAME) {
        Ok(ir::Expr::Print(ir::Print { arg: Box::new(arg) }))
      } else {
        Err(
          diag::ErrorBuilder::from(e.arg.span())
            .title("non printable value")
            .done(),
        )
      }
    }
    ast::Expr::Binary(e) => {
      if let Some((_, operand_ty)) = env.lookup(e.operand.lexeme) {
        match match_fun_ty(2, ids, operand_ty) {
          Err(FuncError::NotCallable) => Err(
            diag::ErrorBuilder::from(e.operand.span)
              .title("not callable")
              .done(),
          ),
          Err(FuncError::WrongNumberOfArgs { expected }) => Err(
            diag::ErrorBuilder::from(e.operand.span)
              .title(format!("expected {} arguments", expected))
              .done(),
          ),
          Ok((param_tys, ret_ty)) => {
            let operand = ir::Name {
              ty: operand_ty.clone(),
              canonical: e.operand.lexeme.to_owned(),
            };

            let left = lower_expr(&*e.left, ids, syms, env, level)?;
            unify(param_tys.get(0).unwrap(), left.as_ty()).ok();

            let right = lower_expr(&*e.right, ids, syms, env, level)?;
            unify(param_tys.get(1).unwrap(), right.as_ty()).ok();

            Ok(ir::Expr::Binary(ir::Binary {
              ty: ret_ty,
              operand,
              left: Box::new(left),
              right: Box::new(right),
            }))
          }
        }
      } else {
        Err(
          diag::ErrorBuilder::from(e.operand.span)
            .title("unknown binary operator")
            .done(),
        )
      }
    }
    ast::Expr::Unary(e) => {
      if let Some((_, operand_ty)) = env.lookup(e.operand.lexeme) {
        match match_fun_ty(1, ids, operand_ty) {
          Err(FuncError::NotCallable) => Err(
            diag::ErrorBuilder::from(e.operand.span)
              .title("not callable")
              .done(),
          ),
          Err(FuncError::WrongNumberOfArgs { expected }) => Err(
            diag::ErrorBuilder::from(e.operand.span)
              .title(format!("expected {} arguments", expected))
              .done(),
          ),
          Ok((param_tys, ret_ty)) => {
            let operand = ir::Name {
              ty: operand_ty.clone(),
              canonical: e.operand.lexeme.to_owned(),
            };

            let right = lower_expr(&*e.right, ids, syms, env, level)?;
            unify(param_tys.get(1).unwrap(), right.as_ty()).ok();

            Ok(ir::Expr::Unary(ir::Unary {
              ty: ret_ty,
              operand,
              right: Box::new(right),
            }))
          }
        }
      } else {
        Err(
          diag::ErrorBuilder::from(e.operand.span)
            .title("unknown unary operator")
            .done(),
        )
      }
    }
    ast::Expr::Name(e) => {
      if let Some((_, ty)) = env.lookup(e.0.lexeme) {
        let mut id_var_map = HashMap::new();
        let ty = instantiate(ids, &mut id_var_map, ty, level);
        let canonical = e.0.lexeme.to_owned();
        Ok(ir::Expr::Name(ir::Name { ty, canonical }))
      } else {
        Err(
          diag::ErrorBuilder::from(e.0.span)
            .title("unknown variable")
            .done(),
        )
      }
    }
    ast::Expr::Integer(e) => Ok(ir::Expr::Integer(ir::Integer {
      ty: Ty::int(),
      repr: e.0.lexeme.into(),
    })),
  }
}

fn lower_item<'src>(
  item: &ast::Item<'src>,
  ids: &mut Ids,
  syms: &mut Symbols,
  env: &mut Env,
  level: Level,
) -> Result<ir::Item, diag::Error> {
  match item {
    ast::Item::Defun(i) => {
      let mut body_env = env.clone();
      let mut param_tys = vec![];
      let mut param_names = vec![];
      for param in &i.params {
        let name = param.0.lexeme;
        let sym = syms.next_local(name);
        let ty = ids.new_var(level);
        body_env.insert(name, sym, ty.clone());
        param_tys.push(ty.clone());
        param_names.push(ir::Name {
          ty,
          canonical: name.to_owned(),
        });
      }

      let body = lower_expr(&i.body, ids, syms, &mut body_env, level)?;
      let canonical = i.name.0.lexeme.to_owned();
      let ret_ty = body.as_ty().to_owned();
      let ty = Ty::Arrow(param_tys, Box::new(ret_ty));
      let name = ir::Name { ty, canonical };
      Ok(ir::Item::Defun(ir::Defun {
        name,
        params: param_names,
        body: Box::new(body),
      }))
    }
  }
}

fn lower_prog<'src>(
  prog: &ast::Prog<'src>,
  ids: &mut Ids,
  syms: &mut Symbols,
  env: &mut Env,
  level: Level,
) -> diag::Result<ir::Prog> {
  let mut items = vec![];
  for item in &prog.items {
    items.push(lower_item(item, ids, syms, env, level)?);
  }
  Ok(ir::Prog { items })
}

fn add_stdlib(syms: &mut Symbols, env: &mut Env) {
  // primitive types
  let int = Ty::Const("int".to_owned());

  // stdlib namespace
  let stdlib_sym = syms.next_root_namespace("stdlib");

  {
    let name = "+".to_owned();
    let sym = syms.next_item(&name, stdlib_sym);
    let ty = Ty::Arrow(vec![int.clone(), int.clone()], Box::new(int));
    env.lexemes.insert(name, (sym, ty));
  }
}

pub fn lower(prog: &ast::Prog) -> Result<ir::Prog, diag::Error> {
  let mut ids = Ids::new();
  let mut syms = Symbols { next: 0 };
  let mut env = Env::new();
  let level = Level::zero();

  add_stdlib(&mut syms, &mut env);

  lower_prog(prog, &mut ids, &mut syms, &mut env, level)
}
