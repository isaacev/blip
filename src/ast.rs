use super::print::{Document, Printable};

#[derive(Debug)]
pub enum Expr<'a> {
  Let(Vec<Binding<'a>>, Box<Expr<'a>>),
  Print(Box<Expr<'a>>),
  Binary(Box<Expr<'a>>, &'a str, Box<Expr<'a>>),
  Name(Name<'a>),
  Integer(u64),
  Float(f64),
}

impl<'a> Printable<'a> for Expr<'a> {
  fn to_doc(&self) -> Document<'a> {
    match self {
      Expr::Let(bindings, body) => Document::new()
        .line_break()
        .indent()
        .write("(let")
        .inc()
        .for_each(bindings.iter(), |b| {
          Document::new()
            .line_break()
            .indent()
            .write("(define ")
            .then(b.0.to_doc())
            .space()
            .inc()
            .then(b.1.to_doc())
            .dec()
            .write(")")
        })
        .line_break()
        .indent()
        .inc()
        .then(body.to_doc())
        .dec()
        .write(")")
        .dec(),
      Expr::Print(expr) => Document::new()
        .write("(print ")
        .then(expr.to_doc())
        .write(")"),
      Expr::Binary(left, oper, right) => Document::new()
        .write("(")
        .write(oper)
        .space()
        .then(left.to_doc())
        .space()
        .then(right.to_doc())
        .write(")"),
      Expr::Name(name) => name.to_doc(),
      Expr::Integer(value) => Document::new().integer(*value),
      Expr::Float(value) => Document::new().float(*value),
    }
  }
}

#[derive(Debug)]
pub struct Binding<'a>(pub Name<'a>, pub Expr<'a>);

#[derive(Debug)]
pub struct Name<'a>(pub &'a str);

impl<'a> Printable<'a> for Name<'a> {
  fn to_doc(&self) -> Document<'a> {
    Document::new().write(self.0)
  }
}
