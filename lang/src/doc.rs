#[derive(Clone)]
enum Inst {
  IncrementIndent,
  DecrementIndent,
  Write(String),
  WriteIndent,
  WriteNewline,
  WriteNewlineIfLineNotBlank,
}

pub trait CanWriteDoc {
  fn write<S: AsRef<str>>(&mut self, s: S);
}

pub struct Doc {
  instructions: Vec<Inst>,
}

pub trait ToDoc {
  fn to_doc(&self) -> Doc;
}

const SINGLE_INDENT: &str = "  ";

impl Doc {
  pub fn new() -> Self {
    Self {
      instructions: vec![],
    }
  }

  pub fn increment_indent(&mut self) {
    self.instructions.push(Inst::IncrementIndent);
  }

  pub fn decrement_indent(&mut self) {
    self.instructions.push(Inst::DecrementIndent);
  }

  pub fn write<S: Into<String>>(&mut self, s: S) {
    self.instructions.push(Inst::Write(s.into()));
  }

  pub fn indent(&mut self) {
    self.instructions.push(Inst::WriteIndent);
  }

  pub fn space(&mut self) {
    self.write(" ");
  }

  pub fn newline(&mut self) {
    self.instructions.push(Inst::WriteNewline);
  }

  pub fn newline_if_not_blank(&mut self) {
    self.instructions.push(Inst::WriteNewlineIfLineNotBlank);
  }

  pub fn then<D: ToDoc>(&mut self, d: &D) {
    self.append(d.to_doc());
  }

  pub fn append(&mut self, mut d: Doc) {
    self.instructions.append(&mut d.instructions);
  }

  pub fn eval<W: CanWriteDoc>(self, w: &mut W) {
    let mut indent = 0;
    let mut on_newline = false;

    for inst in self.instructions {
      match inst {
        Inst::IncrementIndent => indent += 1,
        Inst::DecrementIndent => {
          assert!(indent > 0);
          indent -= 1;
        }
        Inst::Write(s) => {
          if s.len() > 0 {
            on_newline = false;
          }
          w.write(s);
        }
        Inst::WriteIndent => {
          if indent > 0 {
            on_newline = false;
          }
          w.write(SINGLE_INDENT.repeat(indent));
        }
        Inst::WriteNewline => {
          on_newline = true;
          w.write("\n");
        }
        Inst::WriteNewlineIfLineNotBlank => {
          if !on_newline {
            on_newline = true;
            w.write("\n");
          }
        }
      }
    }
  }
}

impl CanWriteDoc for String {
  fn write<S: AsRef<str>>(&mut self, s: S) {
    self.push_str(s.as_ref());
  }
}

impl From<Doc> for String {
  fn from(doc: Doc) -> String {
    let mut s = String::new();
    doc.eval(&mut s);
    s
  }
}
