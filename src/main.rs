extern crate libc;
extern crate llvm_sys;

use std::{env, process};

mod ast;
mod parser;
mod trans;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 {
        println!("Usage: rig INPUT");
        process::exit(1);
    }

    println!("\n---- PARSED ----\n");

    let module = parser::load_and_parse(&args[0]).unwrap().unwrap();
    println!("{:#?}", module);

    println!("\n---- LLVM'D ----\n");

    let llvm_module = trans::translate_module(&module);
    llvm_module.dump();

    trans::write_object_file(&llvm_module, "out.o");
    println!("\nWrote output to out.o.");
}
