
use blip_lang::doc::ToDoc;
use wasm_bindgen::prelude::*;

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

    JsValue::from_serde(&output).unwrap()
}
