extern crate rig;

use rig::{ast, parser, trans};
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

    let mut source = String::new();

    {
        let mut file = try!(File::open(&args[0]));
        try!(file.read_to_string(&mut source));
    }

    println!("\n---- Tokens ----\n");
    let tokens = parser::tokenize(&source);
    let token_kinds: Vec<&parser::TokenKind> = tokens.iter().map(|t| &t.kind).collect();
    println!("{:?}", token_kinds);

    println!("\n---- Parse tree ----\n");
    let fn_def = parser::parse_fn_def(&tokens);
    println!("{:?}", fn_def);

    println!("\n---- LLVM ----\n");
    let module = ast::Module { fns: vec![fn_def.unwrap()] };
    let llvm_module = trans::translate_module(&module);
    llvm_module.dump();
    trans::write_object_file(&llvm_module, "out.o");
    println!("\nWrote output to out.o.");

    Ok(())
}
