use std::io::Write;

use crate::{
    common::{
        arena::{Arena, Handle},
        ast::{Block, Entry, Expr, ExprArena, Intrisinc, Item, Local, Stmt},
        error::Error,
        BinaryOp, Literal, PrimitiveType, Ty, UnaryOp,
    },
    parser::ParseResult,
};
use lasso::RodeoResolver;

const IDENTATION: &str = "    ";

struct BlockContext<'source> {
    expressions: &'source ExprArena,
    locals: &'source Arena<Local>,
}

pub struct PseudoBackend<'source, 'out> {
    parse: &'source ParseResult,
    resolver: &'source RodeoResolver,

    indentation: usize,
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
            indentation: 0,
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

        let mut ctx = BlockContext {
            expressions: &entry.expressions,
            locals: &entry.locals,
        };
        self.emit_block(&mut ctx, &entry.block)?;

        writeln!(self.sink, "Fim.")?;

        Ok(())
    }

    fn emit_block(&mut self, ctx: &mut BlockContext, block: &Block) -> Result<(), Error> {
        self.indentation += 1;

        for (_, stmt) in block.stmts.iter() {
            write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
            match *stmt {
                Stmt::Expr(expr) => self.emit_expression(ctx, expr)?,
                Stmt::LocalInit(local, init) => {
                    let symbol = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(symbol);

                    write!(self.sink, "{} 🠔 ", name)?;
                    self.emit_expression(ctx, init)?
                },
                Stmt::If {
                    condition,
                    ref accept,
                    ref else_ifs,
                    ref reject,
                } => {
                    write!(self.sink, "Se ")?;
                    self.emit_expression(ctx, condition)?;

                    writeln!(self.sink, " Então")?;
                    self.emit_block(ctx, accept)?;

                    write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                    write!(self.sink, "Senão ")?;

                    for else_if in else_ifs.iter() {
                        write!(self.sink, "Se ")?;
                        self.emit_expression(ctx, else_if.condition)?;
                        writeln!(self.sink, " Então")?;
                        self.emit_block(ctx, &else_if.block)?;
                        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                        write!(self.sink, "Senão ")?;
                    }

                    writeln!(self.sink)?;

                    self.emit_block(ctx, reject)?;

                    write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                    write!(self.sink, "FimSe")?;
                },
            }
            writeln!(self.sink, ";")?;
        }

        self.indentation -= 1;

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

    fn emit_expression(&mut self, ctx: &mut BlockContext, expr: Handle<Expr>) -> Result<(), Error> {
        let mut next = expr;
        loop {
            match ctx.expressions[next] {
                Expr::Parenthesized(inner) => {
                    write!(self.sink, "(")?;
                    self.emit_expression(ctx, inner)?;
                    write!(self.sink, ")")?;
                },
                Expr::BinaryOp { lhs, op, rhs } => {
                    self.emit_expression(ctx, lhs)?;
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
                    self.emit_expression(ctx, fun)?;
                    write!(self.sink, "(")?;
                    for (i, &arg) in args.iter().enumerate() {
                        self.emit_expression(ctx, arg)?;
                        if i + 1 != args.len() {
                            write!(self.sink, ", ")?;
                        }
                    }
                    write!(self.sink, ")")?;
                },
                Expr::Assignment { lhs, rhs } => {
                    self.emit_expression(ctx, lhs)?;
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
                    let ident = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(ident);
                    write!(self.sink, "{}", name)?;
                },
                Expr::Intrisinc(intrisinc) => match intrisinc {
                    Intrisinc::In => write!(self.sink, "Ler")?,
                    Intrisinc::Out => write!(self.sink, "Escrever")?,
                },
                Expr::Error => unreachable!(),
            }

            break;
        }

        Ok(())
    }
}
