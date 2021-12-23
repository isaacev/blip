mod err;
mod parse;

const PROGRAM: &'static str = "
let x = 3.14
let y = 2
let z = x + y
";

fn main() {
  parse::parse(PROGRAM);
}
