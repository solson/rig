//! Abstract syntax tree.

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
    pub name: String,
    pub return_ty: Type,
    pub args: Vec<VarDef>,
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
