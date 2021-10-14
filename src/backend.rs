use std::io::Write;

use crate::{
    common::{
        arena::{Arena, Handle},
        ast::{Entry, Expr, ExprArena, Intrisinc, Item, Local, Stmt},
        error::Error,
        BinaryOp, Literal, PrimitiveType, Ty, UnaryOp,
    },
    parser::ParseResult,
};
use lasso::RodeoResolver;

const IDENTATION: &str = "    ";

pub struct PseudoBackend<'source, 'out> {
    parse: &'source ParseResult,
    resolver: &'source RodeoResolver,

    sink: &'out mut dyn Write,
}

impl<'source, 'out> PseudoBackend<'source, 'out> {
    pub fn new(
        parse: &'source ParseResult,
        resolver: &'source RodeoResolver,
        sink: &'out mut dyn Write,
    ) -> Self {
        PseudoBackend {
            parse,
            resolver,
            sink,
        }
    }

    pub fn emit(&mut self) -> Result<(), Error> {
        for item in self.parse.items.iter() {
            match item {
                Item::Entry(entry) => self.emit_entry(entry)?,
            }
        }

        Ok(())
    }

    fn emit_entry(&mut self, entry: &Entry) -> Result<(), Error> {
        let name = self.resolver.resolve(&entry.ident);
        writeln!(self.sink, "Algoritmo {};", name)?;

        if !entry.locals.is_empty() {
            writeln!(self.sink, "Variável")?;
            for (_, local) in entry.locals.iter() {
                let name = self.resolver.resolve(&local.ident);
                write!(self.sink, "{}{}: ", IDENTATION, name)?;
                let ty = self.parse.inference.realize(local.ty)?;
                self.emit_type(ty)?;
                writeln!(self.sink, ";")?;
            }
        }

        writeln!(self.sink, "Início")?;

        for (_, stmt) in entry.block.stmts.iter() {
            write!(self.sink, "{}", IDENTATION)?;
            match *stmt {
                Stmt::Expr(expr) => {
                    self.emit_expression(&entry.expressions, &entry.locals, expr)?
                },
                Stmt::LocalInit(local, init) => {
                    let symbol = &entry.locals[local].ident;
                    let name = self.resolver.resolve(symbol);

                    write!(self.sink, "{} 🠔 ", name)?;
                    self.emit_expression(&entry.expressions, &entry.locals, init)?
                },
            }
            writeln!(self.sink, ";")?;
        }

        writeln!(self.sink, "Fim.")?;

        Ok(())
    }

    fn emit_type(&mut self, ty: Ty) -> Result<(), Error> {
        match ty {
            Ty::Primitive(primitive) => write!(self.sink, "{}", match primitive {
                PrimitiveType::Int => "inteiro",
                PrimitiveType::Float => "real",
                PrimitiveType::Bool => "booleano",
                PrimitiveType::String => "texto",
            })?,
        }

        Ok(())
    }

    fn emit_expression(
        &mut self,
        expressions: &ExprArena,
        locals: &Arena<Local>,
        expr: Handle<Expr>,
    ) -> Result<(), Error> {
        let mut next = expr;
        loop {
            match expressions[next] {
                Expr::Parenthesized(inner) => {
                    write!(self.sink, "(")?;
                    self.emit_expression(expressions, locals, inner)?;
                    write!(self.sink, ")")?;
                },
                Expr::BinaryOp { lhs, op, rhs } => {
                    self.emit_expression(expressions, locals, lhs)?;
                    write!(self.sink, " {} ", match op {
                        BinaryOp::LogicalOr => "or",
                        BinaryOp::LogicalAnd => "and",
                        BinaryOp::Equality => "=",
                        BinaryOp::Inequality => "<>",
                        BinaryOp::Greater => ">",
                        BinaryOp::GreaterEqual => ">=",
                        BinaryOp::Less => "<",
                        BinaryOp::LessEqual => "<=",
                        BinaryOp::BitWiseOr => "or",
                        BinaryOp::BitWiseXor => "xor",
                        BinaryOp::BitWiseAnd => "and",
                        BinaryOp::Addition => "+",
                        BinaryOp::Subtraction => "-",
                        BinaryOp::Multiplication => "*",
                        BinaryOp::Division => "/",
                        BinaryOp::Remainder => "mod",
                    })?;
                    next = rhs;
                    continue;
                },
                Expr::UnaryOp { tgt, op } => {
                    write!(self.sink, "{}", match op {
                        UnaryOp::BitWiseNot => "not",
                        UnaryOp::Negation => "not",
                    })?;
                    next = tgt;
                    continue;
                },
                Expr::Call { fun, ref args } => {
                    self.emit_expression(expressions, locals, fun)?;
                    write!(self.sink, "(")?;
                    for (i, &arg) in args.iter().enumerate() {
                        self.emit_expression(expressions, locals, arg)?;
                        if i + 1 != args.len() {
                            write!(self.sink, ", ")?;
                        }
                    }
                    write!(self.sink, ")")?;
                },
                Expr::Assignment { lhs, rhs } => {
                    self.emit_expression(expressions, locals, lhs)?;
                    write!(self.sink, " 🠔 ")?;
                    next = rhs;
                    continue;
                },
                Expr::Literal(ref literal) => match *literal {
                    Literal::Int(value) => write!(self.sink, "{}", value),
                    Literal::Float(value) => write!(self.sink, "{}", value),
                    Literal::Boolean(value) => write!(self.sink, "{}", value),
                    Literal::String(ref value) => write!(self.sink, "{}", value),
                }?,
                Expr::Variable(local) => {
                    let ident = &locals[local].ident;
                    let name = self.resolver.resolve(&ident);
                    write!(self.sink, "{}", name)?;
                },
                Expr::Intrisinc(intrisinc) => match intrisinc {
                    Intrisinc::In => write!(self.sink, "Ler")?,
                    Intrisinc::Out => write!(self.sink, "Escrever")?,
                },
                Expr::If { .. } => todo!(),
                Expr::Error => unreachable!(),
            }

            break;
        }

        Ok(())
    }
}
