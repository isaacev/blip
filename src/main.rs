mod ast;
mod err;
mod parse;
mod print;

#[cfg(test)]
mod tests;

use print::Printable;

const FILENAME: &'static str = "<example>";
const CONTENTS: &'static str = "
let
  x = 1 + 2 * 3
  y = x * 100
  z = 1 * 2 + 3
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
    Ok(expr) => expr.to_doc().line_break().print(&mut printer),
  }
}
