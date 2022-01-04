use blip_lang::doc::ToDoc;
use blip_lang::*;

fn terminate<D: ToDoc>(code: i32, d: D) -> ! {
    let mut text = String::new();
    d.to_doc().eval(&mut text);
    if code == 0 {
        println!("{}", text);
    } else {
        eprintln!("{}", text);
    }

    std::process::exit(code);
}

pub fn main() {
    if let Some(ref filename) = std::env::args().nth(1) {
        if let Ok(ref contents) = std::fs::read_to_string(filename) {
            let src = err::Source { filename, contents };
            match compile(&src) {
                Err(err) => terminate(1, err),
                Ok(wasm) => terminate(0, wasm),
            }
        } else {
            terminate(
                1,
                blip_lang::diag::ErrorBuilder::new("unable to read file").done(),
            );
        }
    } else {
        terminate(
            1,
            blip_lang::diag::ErrorBuilder::new("no input file").done(),
        );
    }
}
