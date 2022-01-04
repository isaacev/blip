use super::syntax::ir;
use super::ty;
use super::wasm;
use std::collections::HashMap;

#[derive(Debug)]
struct Scope<'a> {
  parent: Option<&'a Scope<'a>>,
  table: HashMap<&'a str, wasm::LocalIndex>,
}

impl<'a> Scope<'a> {
  fn root() -> Self {
    Self {
      parent: None,
      table: HashMap::new(),
    }
  }

  fn subscope(&'a self) -> Scope<'a> {
    Self {
      parent: Some(self),
      table: HashMap::new(),
    }
  }

  fn bind(&mut self, name: &'a str, index: wasm::LocalIndex) {
    self.table.insert(name, index);
  }

  fn lookup(&self, name: &str) -> Option<wasm::LocalIndex> {
    match (self.table.get(name), self.parent) {
      (Some(index), _) => Some(*index),
      (None, Some(parent)) => parent.lookup(name),
      (None, None) => None,
    }
  }
}

fn ir_type_to_wasm_type(ty: &ty::Ty) -> wasm::Type {
  match ty {
    ty::Ty::Const(name) => match name.as_str() {
      "int" => wasm::Type::I32,
      _ => unimplemented!(),
    },
    _ => unimplemented!(),
  }
}

fn compile_expr<'a>(scope: &mut Scope<'a>, expr: &ir::Expr, code: &mut wasm::Code) {
  match expr {
    ir::Expr::Let(e) => {
      let mut subscope = scope.subscope();
      let name = &e.name.canonical;
      let ty = ir_type_to_wasm_type(&e.name.ty);
      let index = code.push_local(ty);
      subscope.bind(name, index);
      compile_expr(&mut subscope, &e.binding, code);
      code.push_inst(wasm::Inst::LocalSet(index));
      compile_expr(&mut subscope, &e.body, code);
    }
    ir::Expr::Binary(e) => {
      compile_expr(scope, &e.left, code);
      compile_expr(scope, &e.right, code);

      match e.operand.canonical.as_str() {
        "+" => code.push_inst(wasm::Inst::I32Add),
        "*" => code.push_inst(wasm::Inst::I32Mul),
        _ => todo!(),
      };
    }
    ir::Expr::Name(e) => {
      let index = scope.lookup(&e.canonical).unwrap();
      code.push_inst(wasm::Inst::LocalGet(index));
    }
    ir::Expr::Integer(e) => {
      let value = e.repr.parse::<i32>().unwrap();
      code.push_inst(wasm::Inst::I32Const(value));
    }
    _ => todo!(),
  }
}

pub fn compile(expr: &ir::Expr) -> wasm::Module {
  let mut module = wasm::Module::new();

  let mut root = Scope::root();
  let mut code = wasm::Code::new();

  compile_expr(&mut root, expr, &mut code);
  code.push_inst(wasm::Inst::Drop);
  code.push_inst(wasm::Inst::BlockEnd);

  let sig = wasm::Type::Func(
    wasm::LengthPrefixedVec::new(),
    wasm::LengthPrefixedVec::new(),
  );

  let func_index = module.add_func(sig, code);
  module.start = Some(func_index);

  module
}
