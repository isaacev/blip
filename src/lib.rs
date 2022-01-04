pub mod codegen;
pub mod diag;
pub mod doc;
pub mod err;
pub mod parse;
pub mod syntax;
pub mod ty;
pub mod wasm;

pub fn compile(src: err::Source) -> Result<wasm::Module, diag::Error> {
  let ast_tree = parse::parse(&src)?;
  let ir_tree = ty::lower(&ast_tree)?;
  let wasm_module = codegen::compile(&ir_tree);
  Ok(wasm_module)
}
