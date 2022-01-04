use blip::doc::ToDoc;
use blip::{diag, err, wasm};
use std::env;
use std::fs;

fn read_and_compile() -> Result<wasm::Module, diag::Error> {
  let filename = env::args()
    .nth(1)
    .ok_or_else(|| diag::ErrorBuilder::new("missing filename").done())?;

  let contents = fs::read_to_string(&filename)
    .map_err(|_| diag::ErrorBuilder::new("unable to read file").done())?;

  let source = err::Source {
    filename: &filename,
    contents: &contents,
  };

  blip::compile(source)
}

fn main() {
  read_and_compile()
    .map(|module| {
      let d = module.to_doc();
      let s: String = d.into();
      println!("{}", s);
    })
    .map_err(|err| {
      let d = err.to_doc();
      let s: String = d.into();
      eprintln!("{}", s);
      std::process::exit(1);
    })
    .ok();
}
