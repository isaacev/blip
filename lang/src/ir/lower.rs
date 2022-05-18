use super::super::diag;
use super::super::parser::ast;
use super::*;
use std::collections::HashMap;

type SymbolId = usize;

pub struct Symbol {
  id: SymbolId,
}

#[derive(Default)]
struct Space {
  types: HashMap<String, Symbol>,
  values: HashMap<String, Symbol>,
}

#[derive(Default)]
struct Names {
  root: Space,
  spaces: Vec<Space>,
  next_symbol_id: SymbolId,
}

impl Names {
  fn next_symbol(&mut self) -> Symbol {
    let id = self.next_symbol_id;
    self.next_symbol_id += 1;
    Symbol { id }
  }

  fn push(&mut self) {
    self.spaces.push(Space::default());
  }

  fn pop(&mut self) -> Space {
    self.spaces.pop().unwrap()
  }
}

fn add_builtin_type(ns: &mut Names, name: &'static str) {
  let sym = ns.next_symbol();
  ns.root.types.insert(name.to_owned(), sym);
}

pub fn root(prog: &ast::Prog<'_>) -> diag::Result<Root> {
  let mut root = Root::default();

  let mut ns = Names::default();
  add_builtin_type(&mut ns, "Int");
  ns.push();

  let mut func = Func::default();
  let mut block = Block::default();

  for item in &prog.items {
    match item {
      ast::Item::Expr(ast::Expr::Integer(expr)) => block.stmts.push(value),
      _ => todo!(),
    }
  }

  ns.pop();
  Ok(Root { funcs: vec![] })
}
