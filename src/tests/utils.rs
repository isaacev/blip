use super::*;

fn is_whitespace(c: &char) -> bool {
  match c {
    ' ' | '\n' | '\t' => true,
    _ => false,
  }
}

fn next_significant_char<I: Iterator<Item = char>>(iter: &mut I) -> Option<char> {
  iter.skip_while(is_whitespace).next()
}

pub fn assert_ast(contents: String, expected: &str) {
  let source = err::Source {
    name: "<example>".to_owned(),
    contents,
  };

  let found: String = match parse::parse(&source) {
    Err(_err) => unreachable!(),
    Ok(expr) => {
      let mut printer = print::StringPrinter::new();
      expr.to_doc().print(&mut printer);
      printer.into()
    }
  };

  let mut found_chars = found.chars();
  let mut expected_chars = expected.chars();

  loop {
    match (
      next_significant_char(&mut found_chars),
      next_significant_char(&mut expected_chars),
    ) {
      (None, None) => break,
      (f, e) if f == e => continue,
      _ => assert_eq!(expected, found),
    }
  }
}
