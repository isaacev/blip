use blip_lang::aux::report::Report;
use blip_lang::*;

fn terminate<R: Into<Report>>(exit_code: i32, reportable: R) -> ! {
  let report: Report = reportable.into();
  if exit_code == 0 {
    println!("{}", report);
  } else {
    eprintln!("{}", report);
  }

  std::process::exit(exit_code);
}

pub fn main() {
  if let Some(ref filename) = std::env::args().nth(1) {
    if let Ok(ref contents) = std::fs::read_to_string(filename) {
      let src = err::Source { filename, contents };
      match compile(&src) {
        Err(err) => terminate(1, &err),
        Ok(wasm) => terminate(0, &wasm),
      }
    } else {
      let err = blip_lang::diag::ErrorBuilder::new("unable to read file").done();
      terminate(1, &err);
    }
  } else {
    let err = blip_lang::diag::ErrorBuilder::new("no input file").done();
    terminate(1, &err);
  }
}
