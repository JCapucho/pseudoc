use crate::{
    common::{
        arena::{Arena, Handle},
        ast::{Block, Expr, ExprArena, Intrisinc, Local, Stmt},
        error::Error,
        BinaryOp, Literal, UnaryOp,
    },
    parser::ParseResult,
};
use lasso::RodeoResolver;
use std::io::Write;

#[derive(Clone, Copy)]
enum NodeId {
    Block(usize, usize),
    Elif(usize, usize, usize),
    End,
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            NodeId::Block(block_id, handle) => write!(f, "stmt_{}_{}", block_id, handle),
            NodeId::Elif(block_id, handle, elif) => {
                write!(f, "stmt_{}_{}_elif_{}", block_id, handle, elif)
            },
            NodeId::End => write!(f, "end"),
        }
    }
}

enum BoxType {
    Box,
    Parallelogram,
}

struct BlockContext<'source> {
    expressions: &'source ExprArena,
    locals: &'source Arena<Local>,
}

pub struct DotBackend<'source, 'out> {
    parse: &'source ParseResult,
    resolver: &'source RodeoResolver,
    block_counter: usize,
    sink: &'out mut dyn Write,
}

impl<'source, 'out> DotBackend<'source, 'out> {
    pub fn new(
        parse: &'source ParseResult,
        resolver: &'source RodeoResolver,
        sink: &'out mut dyn Write,
    ) -> Self {
        DotBackend {
            parse,
            resolver,
            block_counter: 0,
            sink,
        }
    }

    pub fn emit(&mut self) -> Result<(), Error> {
        write!(self.sink, "digraph ")?;
        let name = self.parse.name.map(|ident| self.resolver.resolve(&ident));
        if let Some(name) = name {
            write!(self.sink, "\"{}\" ", name)?;
        }
        writeln!(self.sink, "{{")?;

        writeln!(self.sink, "start [shape=oval, label=\"Início\"]")?;
        writeln!(self.sink, "end [shape=oval, label=\"Fim\"]")?;

        for item in self.parse.items.iter() {
            // TODO
            match *item {}
        }

        let mut ctx = BlockContext {
            expressions: &self.parse.main_block.expressions,
            locals: &self.parse.main_block.locals,
        };
        let block = &self.parse.main_block.block;
        let main_id = self.emit_block(&mut ctx, block, NodeId::End)?;

        let node = if !block.stmts.is_empty() {
            NodeId::Block(main_id, 0)
        } else {
            NodeId::End
        };

        writeln!(self.sink, "start -> {}", node)?;

        writeln!(self.sink, "}}")?;

        Ok(())
    }

    fn emit_block(
        &mut self,
        ctx: &mut BlockContext,
        block: &Block,
        end: NodeId,
    ) -> Result<usize, Error> {
        let block_id = self.block_counter;
        self.block_counter += 1;

        for (handle, stmt) in block.stmts.iter() {
            let node = NodeId::Block(block_id, handle.index());
            write!(self.sink, "{} [label = \"", node)?;

            let next = if handle.index() == block.stmts.len() - 1 {
                end
            } else {
                NodeId::Block(block_id, handle.index() + 1)
            };

            match *stmt {
                Stmt::Expr(expr) => {
                    let box_ty = self.emit_expression(ctx, expr)?;
                    writeln!(self.sink, "\", shape={}]", match box_ty {
                        BoxType::Box => "box",
                        BoxType::Parallelogram => "parallelogram",
                    })?;
                    writeln!(self.sink, "{} -> {}", node, next)?
                },
                Stmt::LocalInit(local, init) => {
                    let symbol = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(symbol);

                    write!(self.sink, "{} = ", name)?;
                    self.emit_expression(ctx, init)?;
                    writeln!(self.sink, "\", shape=box]")?;
                    writeln!(self.sink, "{} -> {}", node, next)?
                },
                Stmt::If {
                    condition,
                    ref accept,
                    ref else_ifs,
                    ref reject,
                } => {
                    self.emit_expression(ctx, condition)?;
                    writeln!(self.sink, "\", shape=diamond]")?;

                    let accept_id = self.emit_block(ctx, accept, next)?;

                    if !accept.stmts.is_empty() {
                        // Write from the selector to the accept block
                        writeln!(
                            self.sink,
                            "{} -> {} [label = \"Se\"]",
                            node,
                            NodeId::Block(accept_id, 0)
                        )?;
                    } else {
                        // Write from the selector to the next node
                        writeln!(self.sink, "{} -> {} [label = \"Se\"]", node, next)?;
                    }

                    let mut else_source = node;
                    for (i, else_if) in else_ifs.iter().enumerate() {
                        let node = NodeId::Elif(block_id, handle.index(), i);
                        write!(self.sink, "{} [label = \"", node)?;
                        self.emit_expression(ctx, else_if.condition)?;
                        writeln!(self.sink, "\", shape=diamond]")?;

                        let else_if_id = self.emit_block(ctx, &else_if.block, next)?;

                        // Write from the previous selector to this with the false label
                        writeln!(self.sink, "{} -> {} [label = \"Senão\"]", else_source, node)?;

                        // Write from the selector to either the body or if it's empty the next
                        // node with the truth label
                        write!(self.sink, "{} -> ", node)?;
                        if !else_if.block.stmts.is_empty() {
                            // The first instruction of the else if block
                            writeln!(
                                self.sink,
                                "{} [label = \"Se\"]",
                                NodeId::Block(else_if_id, 0)
                            )?;
                        } else {
                            writeln!(self.sink, "{} [label = \"Se\"]", next)?;
                        }

                        else_source = node;
                    }

                    let reject_id = self.emit_block(ctx, reject, next)?;

                    if !reject.stmts.is_empty() {
                        // Write from the previous selector to reject block
                        writeln!(
                            self.sink,
                            "{} -> {} [label = \"Senão\"]",
                            else_source,
                            NodeId::Block(reject_id, 0)
                        )?;
                    } else {
                        // Write from the previous selector to the next node
                        writeln!(self.sink, "{} -> {} [label = \"Senão\"]", else_source, next)?;
                    }
                },
            };
        }

        writeln!(self.sink)?;

        Ok(block_id)
    }

    fn emit_expression(
        &mut self,
        ctx: &mut BlockContext,
        expr: Handle<Expr>,
    ) -> Result<BoxType, Error> {
        let mut next = expr;
        loop {
            match ctx.expressions[next] {
                Expr::Parenthesized(inner) => {
                    write!(self.sink, "(")?;
                    let box_type = self.emit_expression(ctx, inner)?;
                    write!(self.sink, ")")?;
                    return Ok(box_type);
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
                    let box_type = self.emit_expression(ctx, fun)?;
                    write!(self.sink, "(")?;
                    for (i, &arg) in args.iter().enumerate() {
                        self.emit_expression(ctx, arg)?;
                        if i + 1 != args.len() {
                            write!(self.sink, ", ")?;
                        }
                    }
                    write!(self.sink, ")")?;
                    return Ok(box_type);
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
                    Literal::String(ref value) => {
                        write!(self.sink, "{}", value.replace("\"", "\\\""))
                    },
                }?,
                Expr::Variable(local) => {
                    let ident = &ctx.locals[local].ident;
                    let name = self.resolver.resolve(ident);
                    write!(self.sink, "{}", name)?;
                },
                Expr::Intrisinc(intrisinc) => {
                    match intrisinc {
                        Intrisinc::In => write!(self.sink, "Ler")?,
                        Intrisinc::Out => write!(self.sink, "Escrever")?,
                    }
                    return Ok(BoxType::Parallelogram);
                },
                Expr::Error => unreachable!(),
            }

            break;
        }

        Ok(BoxType::Box)
    }
}
