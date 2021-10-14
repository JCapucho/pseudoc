pub use lasso::RodeoResolver;

use lasso::{Rodeo as LassoRodeo, Spur};
use std::{
    collections::HashMap,
    fmt,
    hash::{self, Hash},
    ops::Deref,
};

pub mod arena;
pub mod ast;
pub mod error;
pub mod src;

pub type Symbol = Spur;
pub type Rodeo = LassoRodeo<Symbol, fxhash::FxBuildHasher>;
pub type Hasher = fxhash::FxBuildHasher;
pub type FastHashMap<K, V> = HashMap<K, V, Hasher>;

#[derive(Clone, Copy, Debug, Eq)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: src::Span,
}

impl Deref for Ident {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target { &self.symbol }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool { self.symbol == other.symbol }
}

impl Hash for Ident {
    fn hash<H: hash::Hasher>(&self, state: &mut H) { self.symbol.hash(state) }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(val) => write!(f, "{}", val),
            Literal::Float(val) => write!(f, "{}", val),
            Literal::Boolean(val) => write!(f, "{}", val),
            Literal::String(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Copy)]
pub enum BinaryOp {
    LogicalOr,
    LogicalAnd,

    Equality,
    Inequality,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    BitWiseOr,
    BitWiseXor,
    BitWiseAnd,

    Addition,
    Subtraction,

    Multiplication,
    Division,
    Remainder,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::LogicalAnd => write!(f, "&&"),

            BinaryOp::Equality => write!(f, "=="),
            BinaryOp::Inequality => write!(f, "!="),

            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),

            BinaryOp::BitWiseOr => write!(f, "|"),
            BinaryOp::BitWiseXor => write!(f, "^"),
            BinaryOp::BitWiseAnd => write!(f, "&"),

            BinaryOp::Addition => write!(f, "+"),
            BinaryOp::Subtraction => write!(f, "-"),

            BinaryOp::Multiplication => write!(f, "*"),
            BinaryOp::Division => write!(f, "/"),
            BinaryOp::Remainder => write!(f, "%"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Copy)]
pub enum UnaryOp {
    BitWiseNot,
    Negation,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::BitWiseNot => write!(f, "!"),
            UnaryOp::Negation => write!(f, "-"),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Copy, PartialOrd, Ord)]
pub enum PrimitiveType {
    Int = 0,
    Float,
    Bool,
    String,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "int"),
            PrimitiveType::Float => write!(f, "float"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::String => write!(f, "string"),
        }
    }
}

#[derive(Debug)]
pub enum Ty {
    Primitive(PrimitiveType),
}
