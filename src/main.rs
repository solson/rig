extern crate rig;

use rig::{parser, trans};
use std::{env, io, process};
use std::fs::File;
use std::io::prelude::*;

fn main() {
    result_main().unwrap();
}

fn result_main() -> io::Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 {
        println!("Usage: rig INPUT");
        process::exit(1);
    }

    println!("\n---- PARSED ----\n");

    let mut source = String::new();

    {
        let mut file = try!(File::open(&args[0]));
        try!(file.read_to_string(&mut source));
    }

    // parser::

    // let parser = parser::Parser::new(&source);
    // let module = parser.parse_module();
    // println!("{:#?}", module);

    // println!("\n---- LLVM'D ----\n");
    // let llvm_module = trans::translate_module(&module);
    // llvm_module.dump();
    // trans::write_object_file(&llvm_module, "out.o");
    // println!("\nWrote output to out.o.");

    Ok(())
}
