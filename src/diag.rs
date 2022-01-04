use super::doc;
use super::err;
use serde::Serialize;

#[derive(Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Style {
  None,
}

#[derive(Clone, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub struct Span {
  words: Vec<String>,
  style: Style,
}

#[derive(Clone, Serialize)]
pub struct Paragraph {
  spans: Vec<Span>,
}

impl doc::ToDoc for Paragraph {
  fn to_doc(&self) -> doc::Doc {
    todo!()
  }
}

#[derive(Clone, Serialize)]
pub struct Region {
  text: String,
  style: Style,
}

#[derive(Clone, Serialize)]
pub struct Line {
  line_num: usize,
  regions: Vec<Region>,
}

#[derive(Clone, Serialize)]
pub struct Callout {
  start_line: usize,
  start_column: usize,
  end_line: usize,
  end_column: usize,
  style: Style,
  underline: char,
  message: Option<String>,
}

#[derive(Clone, Serialize)]
pub struct Snippet {
  filename: String,
  lines: Vec<Line>,
  callout: Callout,
}

impl Snippet {
  fn new(span: &err::Span<'_>) -> Self {
    use super::parse::lexer::Lexer;
    use super::syntax::token::Token;

    let lines_above = 2;
    let lines_below = 2;
    let first_focused_line = span.start.line;
    let last_focused_line = span.end.line;
    let first_visible_line = if first_focused_line + 1 < lines_above {
      1
    } else {
      first_focused_line - lines_above
    };
    // NOTE: this can be larger than the number of lines in the source file
    let last_visible_line = last_focused_line + lines_below;

    //
    // GROUP TOKENS
    //
    // The first chunk of work involves re-tokenizing the source file and
    // determining which of the tokens are visible in this snippet and grouping
    // the visible tokens based on which line of the file they occupy.
    let mut prev_line: Option<usize> = None;
    let mut visible_lines: Vec<(usize, Vec<Token>)> = vec![];
    for tok in Lexer::new(span.start.source) {
      let tok_start_line = tok.span.start.line;
      let tok_end_line = tok.span.end.line;

      if tok_end_line < first_visible_line {
        // The token ends before the visible region begins so skip it.
        continue;
      }

      if tok_start_line > last_visible_line {
        // The token starts after the visible region. Since tokens are
        // sequential, all subsequent tokens will also be outside the visible
        // region and so the loop can be exited.
        break;
      }

      // If there were blank lines between the prior token and the current
      // token, add those blank lines to the list of visible lines.
      let mut next_unallocated_line = if let Some(prev_line) = prev_line {
        prev_line + 1
      } else {
        tok_start_line
      };
      while next_unallocated_line <= tok_start_line {
        visible_lines.push((next_unallocated_line, vec![]));
        next_unallocated_line += 1;
      }
      prev_line = Some(tok_end_line);

      // Lastly append this token to the last line in the list of visible lines.
      visible_lines.last_mut().unwrap().1.push(tok);
    }

    let mut lines = vec![];
    for (line_num, toks) in visible_lines {
      let mut regions = vec![];
      let mut text = String::new();
      let mut col = 1;
      for tok in toks {
        // Generate however many spaces were between the prior token and this one.
        let spaces = " ".repeat(tok.span.start.column - col);
        col = tok.span.end.column;
        text.push_str(&spaces);

        // Append this token's lexeme.
        text.push_str(tok.lexeme);
      }

      // TODO: when to attach styles to tokens?
      regions.push(Region {
        text,
        style: Style::None,
      });

      lines.push(Line { line_num, regions })
    }

    let filename = span.start.source.filename.to_owned();

    let callout = Callout {
      start_line: span.start.line,
      start_column: span.start.column,
      end_line: span.end.line,
      end_column: span.end.column,
      message: None,
      style: Style::None,
      underline: '~',
    };

    Self {
      filename,
      lines,
      callout,
    }
  }
}

impl doc::ToDoc for Snippet {
  fn to_doc(&self) -> doc::Doc {
    let mut d = doc::Doc::new();
    let gutter_width = self
      .lines
      .iter()
      .map(|l| l.line_num.to_string().len())
      .max()
      .unwrap();

    d.newline_if_not_blank();
    d.indent();
    d.write(format!("{: >g$} : ", " ", g = gutter_width));
    d.write(self.filename.to_owned());
    d.newline();
    for line in self.lines.iter() {
      d.indent();
      d.write(format!("{: >g$} | ", line.line_num, g = gutter_width));

      let mut last_col = 1;
      for reg in line.regions.iter() {
        last_col += reg.text.len();
        d.write(reg.text.to_owned());
        // TODO: apply region styles
      }
      d.newline();

      let line_intersects_callout =
        self.callout.start_line <= line.line_num && line.line_num <= self.callout.end_line;
      if line_intersects_callout {
        let start_col = if self.callout.start_line == line.line_num {
          self.callout.start_column
        } else {
          1
        };

        let end_col = if self.callout.start_line == line.line_num {
          self.callout.end_column
        } else {
          last_col
        };

        let underline_offset = " ".repeat(start_col - 1);
        let underline_text = self
          .callout
          .underline
          .to_string()
          .repeat(end_col - start_col);

        d.indent();
        d.write(format!("{: >g$} | ", "", g = gutter_width));
        d.write(underline_offset);
        d.write(underline_text);
        d.newline();
      }

      // TODO: what if there is a callout?
    }

    d
  }
}

#[derive(Clone, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Section {
  Paragraph(Paragraph),
  Snippet(Snippet),
}

impl doc::ToDoc for Section {
  fn to_doc(&self) -> doc::Doc {
    match self {
      Section::Paragraph(p) => p.to_doc(),
      Section::Snippet(s) => s.to_doc(),
    }
  }
}

#[derive(Serialize)]
pub struct Error {
  title: String,
  sections: Vec<Section>,
}

impl From<ErrorBuilder> for Error {
  fn from(builder: ErrorBuilder) -> Self {
    assert!(builder.title.is_some());

    Self {
      title: builder.title.unwrap(),
      sections: vec![],
    }
  }
}

impl doc::ToDoc for Error {
  fn to_doc(&self) -> doc::Doc {
    let mut d = doc::Doc::new();

    d.increment_indent();
    d.newline_if_not_blank();
    d.indent();
    d.write("ERROR: ");
    d.write(self.title.to_owned());
    d.newline();
    d.newline();
    for section in &self.sections {
      d.then(section);
      d.newline_if_not_blank();
      d.newline();
    }
    d.decrement_indent();

    d
  }
}

pub struct ErrorBuilder {
  title: Option<String>,
  sections: Vec<Section>,
}

impl ErrorBuilder {
  pub fn new<S: AsRef<str>>(title: S) -> Self {
    Self {
      title: Some(title.as_ref().to_owned()),
      sections: vec![],
    }
  }

  pub fn title<S: AsRef<str>>(&mut self, title: S) -> &mut Self {
    self.title = Some(title.as_ref().to_owned());
    self
  }

  pub fn paragraph(&mut self, spans: Vec<Span>) -> &mut Self {
    self.sections.push(Section::Paragraph(Paragraph { spans }));
    self
  }

  pub fn snippet(&mut self, span: &err::Span) -> &mut Self {
    self.sections.push(Section::Snippet(Snippet::new(span)));
    self
  }

  pub fn done(&mut self) -> Error {
    Error {
      title: self.title.clone().unwrap(),
      sections: self.sections.clone(),
    }
  }
}

impl From<err::Span<'_>> for ErrorBuilder {
  fn from(span: err::Span<'_>) -> Self {
    Self {
      title: None,
      sections: vec![Section::Snippet(Snippet::new(&span))],
    }
  }
}

pub type Result<T> = std::result::Result<T, Error>;
