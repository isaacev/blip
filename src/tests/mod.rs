mod utils;

use super::*;

macro_rules! assert_ast {
  ($e:expr => $($tts:tt)*) => {
    utils::assert_ast($e.to_owned(), stringify!($($tts)*));
  };
}

#[test]
fn binary_operator_precedence() {
  assert_ast!("1 + 2 + 3" => (+ (+ 1 2) 3));
  assert_ast!("1 + 2 + 3 + 4" => (+ (+ (+ 1 2) 3) 4));
  assert_ast!("1 + 2 * 3" => (+ 1 (* 2 3)));
  assert_ast!("1 * 2 + 3" => (+ (* 1 2) 3));
}

#[test]
fn unary_operator_precedence() {
  assert_ast!("print 1" => (print 1));
  assert_ast!("print 1 + 2" => (+ (print 1) 2));
  assert_ast!("1 + print 2 + 3" => (+ (+ 1 (print 2)) 3));
}

#[test]
fn let_binding() {
  assert_ast!("let x = 123 in x" =>
    (let
      (define x 123)
      x)
  );
  assert_ast!("
      let
        a = 123
        b = 456
      in
        a * b
      " =>
    (let
      (define a 123)
      (define b 456)
      (* a b))
  );
}
