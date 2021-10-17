use super::{
    arena::{Arena, Handle},
    src::Span,
    BinaryOp, Ident, Literal, UnaryOp,
};
use crate::inference::TypeData;

#[derive(Debug)]
pub enum Item {}

#[derive(Debug, Default)]
pub struct MainBlock {
    pub block: Block,
    pub expressions: ExprArena,
    pub locals: Arena<Local>,
}

#[derive(Debug, Default)]
pub struct Block {
    pub stmts: Arena<Stmt>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Handle<Expr>),
    LocalInit(Handle<Local>, Handle<Expr>),
    If {
        condition: Handle<Expr>,
        accept: Block,
        else_ifs: Vec<ElseIf>,
        reject: Block,
    },
}

#[derive(Debug)]
pub struct ElseIf {
    pub condition: Handle<Expr>,
    pub block: Block,
}

#[derive(Debug)]
pub struct Local {
    pub ident: Ident,
    pub ty: Handle<TypeData>,
    pub init: bool,
}

#[derive(Debug)]
pub enum Expr {
    Parenthesized(Handle<Expr>),
    BinaryOp {
        lhs: Handle<Expr>,
        op: BinaryOp,
        rhs: Handle<Expr>,
    },
    UnaryOp {
        tgt: Handle<Expr>,
        op: UnaryOp,
    },
    Call {
        fun: Handle<Expr>,
        args: Vec<Handle<Expr>>,
    },
    Assignment {
        lhs: Handle<Expr>,
        rhs: Handle<Expr>,
    },
    Literal(Literal),
    Intrisinc(Intrisinc),
    Variable(Handle<Local>),
    Error,
}

#[derive(Debug, Clone, Copy)]
pub enum Intrisinc {
    In,
    Out,
}

#[derive(Debug, Default)]
pub struct ExprArena {
    data: Arena<Expr>,
    info: Vec<Handle<TypeData>>,
}

impl ExprArena {
    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, expr: Expr, span: Span, ty: Handle<TypeData>) -> Handle<Expr> {
        let handle = self.data.append(expr, span);
        self.info.push(ty);
        handle
    }

    pub fn get_type(&self, handle: Handle<Expr>) -> Handle<TypeData> { self.info[handle.index()] }
}

impl std::ops::Deref for ExprArena {
    type Target = Arena<Expr>;

    fn deref(&self) -> &Self::Target { &self.data }
}

impl std::ops::DerefMut for ExprArena {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.data }
}
