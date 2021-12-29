mod err;
mod parse;
mod print;
mod syntax;
mod ty;

#[cfg(test)]
mod tests;

use print::Printable;

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

fn main() {
  let source = err::Source {
    name: FILENAME.to_owned(),
    contents: CONTENTS.to_owned(),
  };

  let mut printer = print::StdoutPrinter::new();
  match parse::parse(&source) {
    Err(err) => eprintln!("{:?}", err),
    Ok(expr) => {
      expr.to_doc().line_break().print(&mut printer);

      match ty::infer(&expr) {
        Err(e) => eprintln!("ERROR: {}", e),
        Ok((t, e)) => {
          println!("{:?}", t);
          println!("{:#?}", e);
        }
      }
    }
  }
}
