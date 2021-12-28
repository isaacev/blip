enum Cmd<'a> {
  Ref(&'a str),
  LineBreak,
  Inc,
  Dec,
  Indent,
}

pub struct Document<'a> {
  cmds: Vec<Cmd<'a>>,
}

impl<'a> Document<'a> {
  pub fn new() -> Self {
    Document { cmds: vec![] }
  }

  pub fn write(mut self, s: &'a str) -> Self {
    self.cmds.push(Cmd::Ref(s));
    self
  }

  pub fn line_break(mut self) -> Self {
    self.cmds.push(Cmd::LineBreak);
    self
  }

  pub fn space(self) -> Self {
    self.write(" ")
  }

  pub fn inc(mut self) -> Self {
    self.cmds.push(Cmd::Inc);
    self
  }

  pub fn dec(mut self) -> Self {
    self.cmds.push(Cmd::Dec);
    self
  }

  pub fn indent(mut self) -> Self {
    self.cmds.push(Cmd::Indent);
    self
  }

  pub fn for_each<T, I: Iterator<Item = T>, F: Fn(T) -> Document<'a>>(
    mut self,
    iter: I,
    f: F,
  ) -> Self {
    iter.for_each(|t| self.cmds.append(&mut f(t).cmds));
    self
  }

  pub fn then(mut self, mut b: Document<'a>) -> Self {
    self.cmds.append(&mut b.cmds);
    self
  }

  pub fn print<P: Printer>(self, printer: &mut P) {
    let mut indent = 0;
    let mut is_blank_line: Option<bool> = None;

    for cmd in self.cmds {
      match cmd {
        Cmd::Ref(str) => {
          printer.write(str);
          is_blank_line = Some(false);
        }
        Cmd::LineBreak => {
          match is_blank_line {
            Some(true) => { /* do nothing */ }
            Some(false) | None => {
              printer.write("\n");
              is_blank_line = Some(true);
            }
          }
        }
        Cmd::Inc => indent += 1,
        Cmd::Dec => {
          assert!(indent > 0, "mismatched indents");
          indent -= 1
        }
        Cmd::Indent => {
          printer.write(format!("{: <width$}", "", width = (indent * 2)));
          is_blank_line = Some(false);
        }
      }
    }
  }
}

pub trait Printable<'a> {
  fn to_doc(&self) -> Document<'a>;
}

pub trait Printer {
  fn write<S: AsRef<str>>(&mut self, s: S);
}

pub struct StdoutPrinter {}

impl StdoutPrinter {
  pub fn new() -> Self {
    StdoutPrinter {}
  }
}

impl Printer for StdoutPrinter {
  fn write<S: AsRef<str>>(&mut self, s: S) {
    print!("{}", s.as_ref());
  }
}

pub struct StringPrinter {
  acc: String,
}

impl StringPrinter {
  #[allow(dead_code)]
  pub fn new() -> Self {
    StringPrinter { acc: String::new() }
  }
}

impl Into<String> for StringPrinter {
  fn into(self) -> String {
    self.acc
  }
}

impl Printer for StringPrinter {
  fn write<S: AsRef<str>>(&mut self, s: S) {
    self.acc.push_str(s.as_ref());
  }
}
