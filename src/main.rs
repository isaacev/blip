mod codegen;
mod err;
mod parse;
mod print;
mod syntax;
mod ty;
mod wasm;

#[cfg(test)]
mod tests;

use std::fs::File;
use std::io;

const FILENAME: &'static str = "<example>";
const CONTENTS: &'static str = "
let
  x = 1 + 2 * 3
in
  let
    y = x * 100
  in
    let
      z = 1 * 2 + 3
    in
      let
        a = let b = 3000 in b
      in
        x + y * a + z
";

fn write_module(m: &wasm::Module) -> std::io::Result<()> {
  use io::Write;
  use wasm::Encode;

  let mut w = io::BufWriter::new(File::create("test.wasm")?);
  w.write_all(&m.encode())
}

fn main() {
  let source = err::Source {
    name: FILENAME.to_owned(),
    contents: CONTENTS.to_owned(),
  };

  match parse::parse(&source) {
    Err(err) => eprintln!("{:?}", err),
    Ok(expr) => match ty::infer(&expr) {
      Err(e) => eprintln!("ERROR: {}", e),
      Ok((_, e)) => match write_module(&codegen::compile(&e)) {
        Err(e) => eprintln!("WRITE ERROR: {}", e),
        Ok(_) => println!("wrote to file"),
      },
    },
  }
}
