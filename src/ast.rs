#[derive(Debug)]
pub enum Expr<'a> {
  Let(Vec<Binding<'a>>, Box<Expr<'a>>),
  Binary(Box<Expr<'a>>, &'a str, Box<Expr<'a>>),
  Name(Name<'a>),
  Integer(u64),
  Float(f64),
}

#[derive(Debug)]
pub struct Binding<'a>(pub Name<'a>, pub Box<Expr<'a>>);

#[derive(Debug)]
pub struct Name<'a>(pub &'a str);
