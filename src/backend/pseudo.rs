use crate::{
    backend::IDENTATION,
    common::{
        arena::{Arena, Handle},
        ast::{Block, Expr, ExprArena, Intrisinc, Local, Stmt},
        error::Error,
        BinaryOp, Literal, PrimitiveType, Ty, UnaryOp,
    },
    parser::ParseResult,
};
use lasso::RodeoResolver;
use std::io::Write;

use super::Config;

struct BlockContext<'source> {
    expressions: &'source ExprArena,
    locals: &'source Arena<Local>,
}

pub struct PseudoBackend<'source, 'out, 'config> {
    parse: &'source ParseResult,
    resolver: &'source RodeoResolver,

    config: &'config Config<'config>,

    indentation: usize,
    sink: &'out mut dyn Write,
}

impl<'source, 'out, 'config> PseudoBackend<'source, 'out, 'config> {
    pub fn new(
        parse: &'source ParseResult,
        resolver: &'source RodeoResolver,
        config: &'config Config<'config>,
        sink: &'out mut dyn Write,
    ) -> Self {
        PseudoBackend {
            parse,
            resolver,
            config,
            indentation: 0,
            sink,
        }
    }

    pub fn emit(&mut self, default_name: &str) -> Result<(), Error> {
        let name = self
            .parse
            .name
            .map(|ident| self.resolver.resolve(&ident))
            .unwrap_or(default_name);
        writeln!(self.sink, "{} {};", self.config.program, name)?;

        for item in self.parse.items.iter() {
            // TODO
            match *item {}
        }

        if !self.parse.main_block.locals.is_empty() {
            writeln!(self.sink, "{}", self.config.variable)?;
            for (_, local) in self.parse.main_block.locals.iter() {
                let name = self.resolver.resolve(&local.ident);
                write!(self.sink, "{}{}: ", IDENTATION, name)?;
                let ty = self.parse.inference.realize(local.ty)?;
                self.emit_type(ty)?;
                writeln!(self.sink, ";")?;
            }
        }

        let mut ctx = BlockContext {
            expressions: &self.parse.main_block.expressions,
            locals: &self.parse.main_block.locals,
        };

        writeln!(self.sink, "{}", self.config.begin)?;
        self.emit_block(&mut ctx, &self.parse.main_block.block)?;
        writeln!(self.sink, "{}.", self.config.end)?;

        Ok(())
    }

    fn emit_block(&mut self, ctx: &mut BlockContext, block: &Block) -> Result<(), Error> {
        self.indentation += 1;

        for (_, stmt) in block.stmts.iter() {
            write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
            match *stmt {
                Stmt::Expr(expr) => {
                    self.emit_expression(ctx, expr)?;
                    writeln!(self.sink, ";")?
                },
                Stmt::LocalInit(local, init) => {
                    let symbol = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(symbol);

                    write!(self.sink, "{} {} ", name, self.config.assign)?;
                    self.emit_expression(ctx, init)?;
                    writeln!(self.sink, ";")?
                },
                Stmt::If {
                    condition,
                    ref accept,
                    ref else_ifs,
                    ref reject,
                } => {
                    write!(self.sink, "{} ", self.config.if_kw)?;
                    self.emit_expression(ctx, condition)?;

                    writeln!(self.sink, " {}", self.config.then_kw)?;
                    if self.config.pascal {
                        self.emit_block_delimited(ctx, accept)?;
                        if reject.stmts.is_empty() {
                            writeln!(self.sink, ";")?;
                        } else {
                            writeln!(self.sink)?;
                        }
                    } else {
                        self.emit_block(ctx, accept)?;
                    }

                    for else_if in else_ifs.iter() {
                        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                        write!(self.sink, "{} {} ", self.config.else_kw, self.config.if_kw)?;
                        self.emit_expression(ctx, else_if.condition)?;
                        writeln!(self.sink, " {}", self.config.then_kw)?;
                        if self.config.pascal {
                            self.emit_block_delimited(ctx, &else_if.block)?;
                            writeln!(self.sink)?;
                        } else {
                            self.emit_block(ctx, &else_if.block)?;
                        }
                    }

                    if !reject.stmts.is_empty() {
                        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                        writeln!(self.sink, "{}", self.config.else_kw)?;
                        if self.config.pascal {
                            self.emit_block_delimited(ctx, reject)?;
                            writeln!(self.sink, ";")?;
                        } else {
                            self.emit_block(ctx, reject)?;
                        }
                    }

                    if !self.config.pascal {
                        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
                        write!(self.sink, "{}", self.config.endif_kw)?;
                        writeln!(self.sink, ";")?;
                    }
                },
            }
        }

        self.indentation -= 1;

        Ok(())
    }

    fn emit_block_delimited(&mut self, ctx: &mut BlockContext, block: &Block) -> Result<(), Error> {
        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
        writeln!(self.sink, "{}", self.config.begin)?;
        self.emit_block(ctx, block)?;
        write!(self.sink, "{}", IDENTATION.repeat(self.indentation))?;
        write!(self.sink, "{}", self.config.end)?;

        Ok(())
    }

    fn emit_type(&mut self, ty: Ty) -> Result<(), Error> {
        match ty {
            Ty::Primitive(primitive) => write!(self.sink, "{}", match primitive {
                PrimitiveType::Int => self.config.int,
                PrimitiveType::Float => self.config.float,
                PrimitiveType::Bool => self.config.boolean,
                PrimitiveType::String => self.config.string,
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
                    write!(self.sink, " {} ", self.config.assign)?;
                    next = rhs;
                    continue;
                },
                Expr::Literal(ref literal) => match *literal {
                    Literal::Int(value) => write!(self.sink, "{}", value),
                    Literal::Float(value) => write!(self.sink, "{}", value),
                    Literal::Boolean(value) => write!(self.sink, "{}", value),
                    Literal::String(ref value) if self.config.pascal => {
                        write!(self.sink, "{}", value.replace("\"", "'"))
                    },
                    Literal::String(ref value) => write!(self.sink, "{}", value),
                }?,
                Expr::Variable(local) => {
                    let ident = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(ident);
                    write!(self.sink, "{}", name)?;
                },
                Expr::Intrisinc(intrisinc) => match intrisinc {
                    Intrisinc::In => write!(self.sink, "{}", self.config.input)?,
                    Intrisinc::Out => write!(self.sink, "{}", self.config.output)?,
                },
                Expr::Error => unreachable!(),
            }

            break;
        }

        Ok(())
    }
}
