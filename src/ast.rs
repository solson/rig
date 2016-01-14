//! Abstract syntax tree.

use intern::Symbol;

#[derive(Clone, Debug)]
pub struct Module {
    pub fns: Vec<FnDef>,
}

#[derive(Clone, Debug)]
pub struct VarDef {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Unit,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: Symbol,
    pub return_ty: Type,
    pub args: Vec<VarDef>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    FnCall {
        func: Symbol,
        args: Vec<Expr>,
    },

    StrLit(String),
}
