pub mod aux;
pub mod diag;
pub mod err;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod ty;
pub mod wasm;

pub fn compile(src: &err::Source) -> Result<ir::Root, diag::Error> {
  let ast_tree = parser::parse(src)?;
  let ir_tree = ir::lower::root(&ast_tree)?;
  Ok(ir_tree)
}
