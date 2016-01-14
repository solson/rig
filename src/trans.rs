use libc;

use llvm_sys::analysis::*;
use llvm_sys::analysis::LLVMVerifierFailureAction::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::target_machine::LLVMCodeGenOptLevel::*;
use llvm_sys::target_machine::LLVMRelocMode::*;
use llvm_sys::target_machine::LLVMCodeModel::*;

use std::{io, ptr};
use std::ffi::{CStr, CString};
use std::io::Write;

use ast;

macro_rules! c_str {
    ($string:expr) => (concat!($string, '\0').as_ptr() as *const ::libc::c_char)
}

pub struct Module {
    context: LLVMContextRef,
    raw: LLVMModuleRef,
}

impl Module {
    pub fn new<T: Into<Vec<u8>>>(name: T) -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let name = CString::new(name).unwrap();
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), context);

            Module {
                context: context,
                raw: module,
            }
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.raw);
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.raw);
            LLVMContextDispose(self.context);
        }
    }
}

pub fn translate_module(ast_module: &ast::Module) -> Module {
    let mut module = Module::new("rig");

    // Declare built-in print function.
    unsafe {
        let i32_type = LLVMInt32TypeInContext(module.context);
        let string_type = LLVMPointerType(LLVMInt8TypeInContext(module.context), 0);
        let mut arg_types = [string_type];
        let puts_type = LLVMFunctionType(i32_type, arg_types.as_mut_ptr(),
                                         arg_types.len() as libc::c_uint, 0);
        LLVMAddFunction(module.raw, c_str!("puts"), puts_type);
    }

    for fn_ in &ast_module.fns {
        translate_fn_def(&mut module, fn_);
    }

    module
}

pub fn translate_fn_def(module: &mut Module, fn_def: &ast::FnDef) {
    unsafe {
        let i32_type = LLVMInt32TypeInContext(module.context);
        let func_type = LLVMFunctionType(i32_type, ptr::null_mut(), 0, 0);

        let func_name = fn_def.name.with(|name| CString::new(name.as_bytes().to_owned()).unwrap());
        let func = LLVMAddFunction(module.raw, func_name.as_ptr(), func_type);

        let bb = LLVMAppendBasicBlockInContext(module.context, func, c_str!("entry"));
        let builder = LLVMCreateBuilderInContext(module.context);
        LLVMPositionBuilderAtEnd(builder, bb);

        for expr in &fn_def.body {
            translate_expr(expr, module, builder);
        }

        LLVMBuildRet(builder, LLVMConstInt(i32_type, 0, 0));

        LLVMVerifyModule(module.raw, LLVMPrintMessageAction, ptr::null_mut());
        LLVMDisposeBuilder(builder);
    }
}

fn translate_expr(expr: &ast::Expr, module: &Module, builder: LLVMBuilderRef) -> LLVMValueRef {
    match *expr {
        ast::Expr::FnCall { ref func, ref args } => {
            let mut arg_values: Vec<LLVMValueRef> = args
                .iter()
                .map(|arg| translate_expr(arg, module, builder))
                .collect();

            if func.with(|name| name == "print") {
                unsafe {
                    let func = LLVMGetNamedFunction(module.raw, c_str!("puts"));
                    LLVMBuildCall(builder, func, arg_values.as_mut_ptr(),
                                  arg_values.len() as libc::c_uint, c_str!(""))
                }
            } else {
                unimplemented!()
            }
        }

        ast::Expr::StrLit(ref string) => {
            let cstr = CString::new(string.clone()).unwrap();
            unsafe {
                LLVMBuildGlobalStringPtr(builder, cstr.as_ptr(), c_str!(".str"))
            }
        }
    }
}

pub fn write_object_file(module: &Module, filename: &str) {
    unsafe {
        // TODO(tsion): Move this elsewhere.
        assert!(LLVM_InitializeNativeTarget() == 0);
        assert!(LLVM_InitializeNativeAsmPrinter() == 0);

        let triple = LLVMGetDefaultTargetTriple();
        let mut target = ptr::null_mut();
        assert!(LLVMGetTargetFromTriple(triple, &mut target, ptr::null_mut()) == 0);
        let target_machine = LLVMCreateTargetMachine(
            target,
            triple,
            c_str!(""),
            c_str!(""),
            LLVMCodeGenLevelNone,
            LLVMRelocDefault,
            LLVMCodeModelDefault);

        let filename_cstr = CString::new(filename).unwrap();
        let mut error = ptr::null_mut();
        let result = LLVMTargetMachineEmitToFile(
            target_machine,
            module.raw,
            filename_cstr.as_ptr() as *mut libc::c_char,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut error);

        if result != 0 {
            println!("error: failed to write output file '{}'", filename);
            if !error.is_null() {
                print!("error: ");
                io::stdout().write(CStr::from_ptr(error).to_bytes()).unwrap();
                println!("");
                LLVMDisposeMessage(error);
            }
            return;
        }

        LLVMDisposeTargetMachine(target_machine);
    }
}
