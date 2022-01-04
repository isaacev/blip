use wasm_bindgen::prelude::*;

mod codegen;
mod diag;
mod doc;
mod err;
mod parse;
mod syntax;
mod ty;
mod wasm;

#[wasm_bindgen]
#[cfg(target_arch = "wasm32")]
pub fn compile_for_js(contents: &str) -> JsValue {
  let src = err::Source {
    filename: "example",
    contents,
  };

  JsValue::from_serde(&compile(&src)).unwrap()
}

pub fn compile(src: &err::Source) -> Result<String, diag::Error> {
  let ast_tree = parse::parse(src)?;
  let ir_tree = ty::lower(&ast_tree)?;
  let wasm_module = codegen::compile(&ir_tree);
  let mut text = String::new();

  use doc::ToDoc;
  wasm_module.to_doc().eval(&mut text);
  Ok(text)
}
