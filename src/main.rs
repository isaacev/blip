mod ast;
mod err;
mod parse;

const FILENAME: &'static str = "<example>";
const CONTENTS: &'static str = "
let
  x = 3.14 + 2.
  y = 2
  z = x + y
  a = z >= 3
in
  x + y + z
";

fn main() {
  let source = err::Source {
    name: FILENAME,
    contents: CONTENTS,
  };
  parse::parse(&source);
}
