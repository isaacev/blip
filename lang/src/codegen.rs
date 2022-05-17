use crate::wasm::FuncIndex;

use super::ir;
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
    ir::Expr::Print(e) => {
      compile_expr(scope, &e.arg, code);
      code.push_inst(wasm::Inst::Call(wasm::FuncIndex::new(0)));
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
    ir::Expr::Unary(..) => unimplemented!(),
    ir::Expr::Name(e) => {
      let index = scope.lookup(&e.canonical).unwrap();
      code.push_inst(wasm::Inst::LocalGet(index));
    }
    ir::Expr::Integer(e) => {
      let value = e.repr.parse::<i32>().unwrap();
      code.push_inst(wasm::Inst::I32Const(value));
    }
  }
}

fn compile_sig(ty: &ty::Ty) -> wasm::Type {
  match ty {
    ty::Ty::Var(cell) => {
      let var = cell.borrow();
      match &*var {
        ty::Var::Link(ty) => compile_sig(&ty),
        ty::Var::Generic(..) => unimplemented!(),
        ty::Var::Unbound(..) => unimplemented!(),
      }
    }
    ty::Ty::Const(name) => match name.as_str() {
      "int" => wasm::Type::I32,
      name => unimplemented!("{}", name),
    },
    ty::Ty::Arrow(params, ret) => {
      let mut p = wasm::LengthPrefixedVec::new();
      for param in params {
        p.push(compile_sig(param));
      }

      let mut r = wasm::LengthPrefixedVec::new();
      if !ret.is_const(ty::VOID_NAME) {
        r.push(compile_sig(&*ret));
      }

      wasm::Type::Func(p, r)
    }
  }
}

fn compile_item(item: &ir::Item, module: &mut wasm::Module) -> wasm::FuncIndex {
  match item {
    ir::Item::Defun(i) => {
      let mut code = wasm::Code::new();
      let mut scope = Scope::root();

      for param in &i.params {
        let ty = compile_sig(&param.ty);
        scope.bind(&param.canonical, code.push_local(ty));
      }

      compile_expr(&mut scope, &i.body, &mut code);
      let sig = compile_sig(&i.name.ty);
      module.add_func(sig, code)
    }
  }
}

fn compile_prog(prog: &ir::Prog, module: &mut wasm::Module) -> Option<wasm::FuncIndex> {
  let mut func_index = None;
  for item in &prog.items {
    func_index = Some(compile_item(item, module));
  }
  func_index
}

macro_rules! intrinsic {
  ($module:expr , $name:expr , $in:ident , $out:ident) => {
    let ty = wasm::Type::Func(vec![wasm::Type::$in].into(), vec![wasm::Type::$out].into());
    let idx = $module.type_section.push(ty);
    $module
      .import_section
      .push("", $name, wasm::ImportDesc::TypeIndex(idx));
  };
}

fn import_intrinsics(module: &mut wasm::Module) {
  intrinsic!(module, "log", I64, I64);
}

pub fn compile(prog: &ir::Prog) -> wasm::Module {
  let mut module = wasm::Module::new();
  import_intrinsics(&mut module);

  let log_sig = compile_sig(&ty::Ty::Arrow(vec![ty::Ty::int()], Box::new(ty::Ty::int())));
  module.type_section.push(log_sig);

  let main_index = compile_prog(prog, &mut module);

  module
}
