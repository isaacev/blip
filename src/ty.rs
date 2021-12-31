use super::syntax::{ast, ir};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

trait Union {
  fn union(&self, other: &Self) -> Self;
}

impl<K, V> Union for HashMap<K, V>
where
  K: Clone + Eq + Hash,
  V: Clone,
{
  fn union(&self, other: &Self) -> Self {
    let mut res = self.clone();
    for (key, value) in other {
      res.entry(key.clone()).or_insert(value.clone());
    }
    res
  }
}

/// A trait common to al things considered types or type-like.
trait Types {
  /// Find the set of free variables in a type.
  fn find_free_type_vars(&self) -> HashSet<Var>;

  /// Applies a substitution to a type.
  fn apply(&self, subst: &Substitutions) -> Self;
}

impl<'a, T> Types for Vec<T>
where
  T: Types,
{
  // The free type variables of a vector of types is the union of the free type
  // variables of each of the types in the vector.
  fn find_free_type_vars(&self) -> HashSet<Var> {
    self
      .iter()
      .map(|x| x.find_free_type_vars())
      .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
  }

  // To apply a substitution to a vector of types, just apply to each type in
  // the vector.
  fn apply(&self, s: &Substitutions) -> Vec<T> {
    self.iter().map(|x| x.apply(s)).collect()
  }
}

/// A mapping from type variables to types.
#[derive(Clone)]
struct Substitutions(HashMap<Var, Type>);

impl Substitutions {
  fn new() -> Self {
    Self(HashMap::new())
  }

  fn compose(&self, other: &Substitutions) -> Substitutions {
    Substitutions(
      self.union(
        &other
          .iter()
          .map(|(k, v)| (k.clone(), v.apply(self)))
          .collect(),
      ),
    )
  }
}

impl Deref for Substitutions {
  type Target = HashMap<Var, Type>;

  fn deref(&self) -> &HashMap<Var, Type> {
    &self.0
  }
}

impl DerefMut for Substitutions {
  fn deref_mut(&mut self) -> &mut HashMap<Var, Type> {
    &mut self.0
  }
}

pub struct TypeError {
  msg: String,
}

impl TypeError {
  fn new(msg: String) -> Self {
    Self { msg }
  }
}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.msg)
  }
}

type TypeResult<T> = std::result::Result<T, TypeError>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
  id: usize,
}

impl Var {
  fn bind(&self, ty: &Type) -> TypeResult<Substitutions> {
    // Check for binding a variable to itself
    if let Type::Var(ref v) = ty {
      if v == self {
        return Ok(Substitutions::new());
      }
    }

    // the 'occurs' check prevents illegal recursive types
    if ty.find_free_type_vars().contains(self) {
      return Err(TypeError::new(format!(
        "occur check fails: {} vs {}",
        self, ty
      )));
    }

    let mut s = Substitutions::new();
    s.insert(self.clone(), ty.clone());
    Ok(s)
  }
}

impl std::fmt::Display for Var {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "'t{}", self.id)
  }
}

pub struct Vars {
  next_id: Cell<usize>,
}

impl Vars {
  fn new() -> Self {
    Self {
      next_id: Cell::new(0),
    }
  }

  fn next(&self) -> Var {
    let id = self.next_id.get();
    self.next_id.set(id + 1);
    Var { id }
  }
}

#[derive(Debug, Clone)]
pub enum Type {
  Const(String),
  Var(Var),
  Fun(Box<Type>, Box<Type>),
}

impl Type {
  // A substitution S such that S(self) is congruent to S(other)
  fn most_general_unifier(&self, other: &Type) -> TypeResult<Substitutions> {
    match (self, other) {
      // For functions, find the most general unifier for the inputs, apply the
      // resulting substitution to the outputs, find the outputs' most general
      // unifier, and finally compose the two resulting substitutions.
      (Type::Fun(i1, o1), Type::Fun(i2, o2)) => {
        let sub1 = i1.most_general_unifier(&*i2)?;
        let sub2 = o1.apply(&sub1).most_general_unifier(&o2.apply(&sub1))?;
        Ok(sub1.compose(&sub2))
      }

      // If one of the types is a variable, the variable can be bound to the
      // other type. This also handles the case where both are variables.
      (Type::Var(v), t) => v.bind(t),
      (t, Type::Var(v)) => v.bind(t),

      // If they are both primitives, make sure they are the same primitive.
      (Type::Const(a), Type::Const(b)) if a == b => Ok(Substitutions::new()),

      (a, b) => Err(TypeError::new(format!("unable to unify {} and {}", a, b))),
    }
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Type::Const(name) => write!(f, "{}", name),
      Type::Var(var) => write!(f, "{}", var),
      Type::Fun(i, o) => write!(f, "({} → {})", i, o),
    }
  }
}

impl Types for Type {
  fn find_free_type_vars(&self) -> HashSet<Var> {
    match self {
      Type::Var(ref v) => [v.clone()].iter().cloned().collect(),
      Type::Const(_) => HashSet::new(),
      Type::Fun(i, o) => i
        .find_free_type_vars()
        .union(&o.find_free_type_vars())
        .cloned()
        .collect(),
    }
  }

  fn apply(&self, s: &Substitutions) -> Type {
    match self {
      Type::Var(v) => s.get(v).cloned().unwrap_or(self.clone()),
      Type::Fun(i, o) => Type::Fun(Box::new(i.apply(s)), Box::new(o.apply(s))),
      _ => self.clone(),
    }
  }
}

/// A polytype is a type in which there are a number of forall quantifiers, i.e.
/// some parts of the type may not be concrete but instead correct for all
/// possible types.
#[derive(Debug, Clone)]
pub struct Polytype {
  pub vars: Vec<Var>,
  pub ty: Type,
}

impl Types for Polytype {
  /// The free type variables in a polytype are those that are free in the
  /// internal type and not bound by the variable mapping.
  fn find_free_type_vars(&self) -> HashSet<Var> {
    self
      .ty
      .find_free_type_vars()
      .difference(&self.vars.iter().cloned().collect())
      .cloned()
      .collect()
  }

  fn apply(&self, s: &Substitutions) -> Polytype {
    Polytype {
      vars: self.vars.clone(),
      ty: {
        let mut sub = s.clone();
        for var in &self.vars {
          sub.remove(var);
        }
        self.ty.apply(&sub)
      },
    }
  }
}

impl Polytype {
  /// Instantiates a polytype into a type. Replaces all bound type variables
  /// with fresh type variables and return the resulting type.
  fn instantiate(&self, vars: &Vars) -> Type {
    let new_vars = self.vars.iter().map(|_| Type::Var(vars.next()));
    self.ty.apply(&Substitutions(
      self.vars.iter().cloned().zip(new_vars).collect(),
    ))
  }
}

#[derive(Debug, Clone)]
struct Env(HashMap<String, Polytype>);

impl Deref for Env {
  type Target = HashMap<String, Polytype>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Env {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Types for Env {
  fn find_free_type_vars(&self) -> HashSet<Var> {
    self
      .values()
      .map(|x| x.clone())
      .collect::<Vec<Polytype>>()
      .find_free_type_vars()
  }

  fn apply(&self, s: &Substitutions) -> Env {
    Env(self.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
  }
}

impl Env {
  fn new() -> Self {
    Env(HashMap::new())
  }

  fn generalize(&self, ty: &Type) -> Polytype {
    Polytype {
      vars: ty
        .find_free_type_vars()
        .difference(&self.find_free_type_vars())
        .cloned()
        .collect(),
      ty: ty.clone(),
    }
  }

  fn infer(
    &self,
    exp: &ast::Expr,
    vars: &Vars,
    names: &ir::Names,
  ) -> TypeResult<(Substitutions, Type, ir::Expr)> {
    match exp {
      ast::Expr::Paren(e) => self.infer(&e.expr, vars, names),

      ast::Expr::Let(e) => {
        let lexeme = e.name.0.lexeme;
        let mut env = self.clone();
        env.remove(lexeme);
        let (binding_substitutions, binding_type, binding) = env.infer(&e.binding, vars, names)?;
        let name = names.next(lexeme, binding_type.clone());

        let binding_polytype = env.apply(&binding_substitutions).generalize(&binding_type);
        env.insert(lexeme.to_owned(), binding_polytype);
        let (body_substitutions, let_type, body) = env
          .apply(&binding_substitutions)
          .infer(&e.body, vars, names)?;

        let let_substitutions = body_substitutions.compose(&binding_substitutions);
        let let_expr = ir::Expr::Let(ir::Let {
          name,
          binding: Box::new(binding),
          body: Box::new(body),
        });

        Ok((let_substitutions, let_type, let_expr))
      }

      ast::Expr::Binary(e) => {
        let lexeme = e.operand.lexeme;

        // Find the operand type signature
        let (operand_subst, operand_type) = match self.get(lexeme) {
          Some(s) => Ok((Substitutions::new(), s.instantiate(vars))),
          None => Err(TypeError::new(format!("unknown operator {}", lexeme))),
        }?;
        let operand = names.next(lexeme, operand_type.clone());

        // Apply the left operand
        let (left_subst, left_type, left) =
          self.apply(&operand_subst).infer(&e.left, vars, &names)?;
        let tv = Type::Var(vars.next());
        let s3 = operand_type
          .apply(&left_subst)
          .most_general_unifier(&Type::Fun(Box::new(left_type), Box::new(tv.clone())))?;

        // Apply the right operand
        let (u1, v1) = (
          s3.compose(&left_subst.compose(&operand_subst)),
          tv.apply(&s3),
        );
        let (right_subst, right_type, right) = self.apply(&u1).infer(&e.right, vars, &names)?;
        let tv = Type::Var(vars.next());
        let u3 = v1
          .apply(&right_subst)
          .most_general_unifier(&Type::Fun(Box::new(right_type), Box::new(tv.clone())))?;

        let binary_subst = u3.compose(&right_subst.compose(&u1));
        let binary_type = tv.apply(&u3);

        let binary_expr = ir::Expr::Binary(ir::Binary {
          operand,
          left: Box::new(left),
          right: Box::new(right),
        });

        Ok((binary_subst, binary_type, binary_expr))
      }

      ast::Expr::Unary(e) => {
        let lexeme = e.operand.lexeme;

        // Find the operand type signature
        let (operand_subst, operand_type) = match self.get(lexeme) {
          Some(s) => Ok((Substitutions::new(), s.instantiate(vars))),
          None => Err(TypeError::new(format!("unknown operator {}", lexeme))),
        }?;
        let operand = names.next(lexeme, operand_type.clone());

        let (right_subst, right_type, right) =
          self.apply(&operand_subst).infer(&e.right, vars, &names)?;
        let tv = Type::Var(vars.next());
        let s3 = operand_type
          .apply(&right_subst)
          .most_general_unifier(&Type::Fun(Box::new(right_type), Box::new(tv.clone())))?;

        let unary_subst = s3.compose(&right_subst.compose(&operand_subst));
        let unary_type = tv.apply(&s3);
        let unary_expr = ir::Expr::Unary(ir::Unary {
          operand,
          right: Box::new(right),
        });

        Ok((unary_subst, unary_type, unary_expr))
      }

      ast::Expr::Name(v) => match self.get(v.0.lexeme) {
        Some(s) => {
          let name_subst = Substitutions::new();
          let name_type = s.instantiate(vars);
          let name_expr = ir::Expr::Name(names.next(v.0.lexeme, name_type.clone()));
          Ok((name_subst, name_type, name_expr))
        }
        None => {
          return Err(TypeError::new(format!(
            "unbound variable: {} at {}",
            v.0.lexeme, v.0.span.start
          )));
        }
      },
      ast::Expr::Integer(e) => {
        let int_subst = Substitutions::new();
        let int_type = Type::Const("int".to_owned());
        let int_expr = ir::Expr::Integer(ir::Integer {
          repr: e.0.lexeme.to_owned(),
        });
        Ok((int_subst, int_type, int_expr))
      }
      ast::Expr::Float(e) => {
        let float_subst = Substitutions::new();
        let float_type = Type::Const("float".to_owned());
        let float_expr = ir::Expr::Float(ir::Float {
          repr: e.0.lexeme.to_owned(),
        });
        Ok((float_subst, float_type, float_expr))
      }
    }
  }
}

fn inject_stdlib(env: &mut Env) {
  let i = Type::Const("int".to_owned());

  env.insert(
    "+".into(),
    Polytype {
      vars: Vec::new(),
      ty: Type::Fun(
        Box::new(i.clone()),
        Box::new(Type::Fun(Box::new(i.clone()), Box::new(i.clone()))),
      ),
    },
  );

  env.insert(
    "*".into(),
    Polytype {
      vars: Vec::new(),
      ty: Type::Fun(
        Box::new(i.clone()),
        Box::new(Type::Fun(Box::new(i.clone()), Box::new(i.clone()))),
      ),
    },
  );
}

pub fn infer(expr: &ast::Expr) -> TypeResult<(Type, ir::Expr)> {
  let mut env = Env::new();
  inject_stdlib(&mut env);
  let vars = Vars::new();
  let names = ir::Names::new();
  let (s, t, e) = env.infer(expr, &vars, &names)?;
  Ok((t.apply(&s), e))
}
