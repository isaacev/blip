use std::fmt;

pub trait ReportWriter {
  fn write<S: AsRef<str>>(&mut self, s: S);
}

#[derive(Clone)]
enum Effect {
  IncrementIndent,
  DecrementIndent,
  Write(String),
  WriteIndent,
  WriteNewline,
  WriteNewlineIfLineNotBlank,
}

#[derive(Default)]
pub struct Report {
  effects: Vec<Effect>,
}

macro_rules! write_util {
  ($name:ident,  $write:expr ) => {
    pub fn $name(&mut self) {
      self.write($write);
    }
  };
}

impl Report {
  pub fn increment_indent(&mut self) {
    self.effects.push(Effect::IncrementIndent);
  }

  pub fn decrement_indent(&mut self) {
    self.effects.push(Effect::DecrementIndent);
  }

  pub fn write<S: AsRef<str>>(&mut self, s: S) {
    self.effects.push(Effect::Write(s.as_ref().to_owned()));
  }

  pub fn indent(&mut self) {
    self.effects.push(Effect::WriteIndent);
  }

  pub fn newline(&mut self) {
    self.effects.push(Effect::WriteNewline);
  }

  pub fn newline_if_not_blank(&mut self) {
    self.effects.push(Effect::WriteNewlineIfLineNotBlank);
  }

  pub fn then_from<R: Into<Report>>(&mut self, reportable: R) {
    self.then(&reportable.into())
  }

  pub fn then(&mut self, report: &Self) {
    for e in report.effects.iter() {
      self.effects.push(e.clone());
    }
  }

  pub fn for_each<T, I, F>(&mut self, i: I, f: F)
  where
    I: Iterator<Item = T>,
    F: Fn(T) -> Self,
  {
    for t in i {
      self.then(&(f(t)));
    }
  }

  pub fn if_some<T, F>(&mut self, cond: &Option<T>, f: F)
  where
    F: Fn(&T) -> Self,
  {
    if let Some(t) = cond {
      self.then(&f(t));
    }
  }

  //
  // Additional utility methods
  //
  write_util!(space, " ");
  write_util!(paren_left, "(");
  write_util!(paren_right, ")");
}

const SINGLE_INDENT: &str = "  ";

impl fmt::Display for Report {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut indent = 0;
    let mut on_newline = false;

    for effect in &self.effects {
      match effect {
        Effect::IncrementIndent => indent += 1,
        Effect::DecrementIndent => {
          assert!(indent > 0);
          indent -= 1;
        }
        Effect::Write(s) => {
          if !s.is_empty() {
            on_newline = false;
          }
          write!(f, "{}", s)?;
        }
        Effect::WriteIndent => {
          if indent > 0 {
            on_newline = false;
          }
          write!(f, "{}", SINGLE_INDENT.repeat(indent))?;
        }
        Effect::WriteNewline => {
          on_newline = true;
          writeln!(f)?;
        }
        Effect::WriteNewlineIfLineNotBlank => {
          if !on_newline {
            on_newline = true;
            writeln!(f)?;
          }
        }
      }
    }

    Ok(())
  }
}

macro_rules! report {
  ($($name:ident $( ( $($arg:expr),* ) )? )*) => {{
    #[allow(unused_mut)]
    let mut r = crate::aux::report::Report::default();
    $(r.$name( $( $($arg),* )? );)*
    r
  }};
}

pub(crate) use report;
