//! Abstract syntax tree.

#[derive(Clone, Debug)]
pub struct Module {
    pub fns: Vec<FnDef>,
}

#[derive(Clone, Debug)]
pub struct VarDef {
    pub name: String,
    // type
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: String,
    pub args: Vec<VarDef>,
    // return type
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    FnCall {
        func: String,
        args: Vec<Expr>,
    },

    StrLit(String),
}
