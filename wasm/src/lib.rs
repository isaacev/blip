use blip_lang::doc::ToDoc;
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
enum CompilerResult {
  Ok(String),
  Err(blip_lang::diag::Error),
}

#[wasm_bindgen]
pub fn compile_for_js(contents: &str) -> JsValue {
  let src = blip_lang::err::Source {
    filename: "example",
    contents,
  };

  let output = blip_lang::compile(&src).map(|wasm_module| {
    let mut text = String::new();
    wasm_module.to_doc().eval(&mut text);
    text
  });

  let normalized = match output {
    Ok(text) => CompilerResult::Ok(text),
    Err(err) => CompilerResult::Err(err),
  };

  JsValue::from_serde(&normalized).unwrap()
}
