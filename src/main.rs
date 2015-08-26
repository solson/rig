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

    let fn_def = parser::load_and_parse(&args[0]).unwrap().unwrap();
    println!("{:?}", fn_def);

    println!("\n---- LLVM'D ----\n");

    let module = trans::translate_fn_def(&fn_def);
    module.dump();

    trans::write_object_file(&module, "out.o");
    println!("\nWrote output to out.o.");
}
