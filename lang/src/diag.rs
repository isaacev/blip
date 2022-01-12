use super::aux::report::{report, Report};
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
pub struct Region {
  text: String,
  kind: Option<super::lexer::tokens::Kind>,
}

#[derive(Clone, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Line {
  Source {
    line_num: usize,
    regions: Vec<Region>,
  },
  Callout {
    start_column: usize,
    width: usize,
  },
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
    use super::lexer::{tokens::Token, Lexer};

    let lines_above = 2;
    let lines_below = 2;
    let first_focused_line = span.start.line;
    let last_focused_line = span.end.line;
    let first_visible_line = if first_focused_line < lines_above {
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
      let mut col = 1;
      for tok in toks {
        // Generate however many spaces were between the prior token and this one.
        let total_spaces = tok.span.start.column - col;
        col = tok.span.end.column;
        if total_spaces > 0 {
          let text = " ".repeat(total_spaces);
          regions.push(Region { text, kind: None });
        }

        // Append this token's lexeme.
        let text = tok.lexeme.to_owned();
        let kind = Some(tok.kind);
        regions.push(Region { text, kind });
      }

      lines.push(Line::Source { line_num, regions });

      let line_intersects_callout = span.start.line <= line_num && line_num <= span.end.line;
      if line_intersects_callout {
        let start_column = if span.start.line == line_num {
          span.start.column
        } else {
          1
        };

        let width = if span.start.line == line_num {
          span.end.column - start_column
        } else {
          col
        };

        lines.push(Line::Callout {
          start_column,
          width,
        });
      }
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

impl Into<Report> for &Snippet {
  fn into(self) -> Report {
    let gutter_width = self
      .lines
      .iter()
      .map(|l| {
        if let Line::Source { line_num, .. } = l {
          *line_num
        } else {
          0
        }
      })
      .max()
      .unwrap();

    report! {
      newline
      indent
      write(format!("{: >g$} : ", " ", g = gutter_width))
      write(&self.filename)
      newline
      for_each(self.lines.iter(), { |line|
        match line {
          Line::Source { line_num, regions } => report!{
            indent
            write(format!("{: >g$} | ", line_num, g = gutter_width))
            for_each(regions.iter(), { |reg|
              report!(write(&reg.text))
            })
            newline
          },
          Line::Callout { start_column, width } => {
            let underline_offset = " ".repeat(start_column - 1);
            let underline_text = self.callout.underline.to_string().repeat(*width);

            report! {
              indent
              write(format!("{: >g$} | ", "", g = gutter_width))
              write(underline_offset)
              write(underline_text)
              newline
            }
          }
        }
      })
    }
  }
}

#[derive(Clone, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Section {
  Snippet(Snippet),
}

impl Into<Report> for &Section {
  fn into(self) -> Report {
    match self {
      Section::Snippet(s) => s.into(),
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

impl Into<Report> for &Error {
  fn into(self) -> Report {
    report! {
      increment_indent
      newline_if_not_blank
      indent
      write("ERROR:")
      space
      write(&self.title)
      newline
      for_each(self.sections.iter(), |sec| sec.into())
      decrement_indent
    }
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
