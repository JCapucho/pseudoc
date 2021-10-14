use crate::{
    common::{
        arena::{Arena, Handle},
        ast::*,
        error::{Error, ErrorKind},
        src::Span,
        BinaryOp, FastHashMap, Ident, Literal, Symbol, UnaryOp,
    },
    inference::{Inference, TypeData},
    lexer::{Lexer, Token},
};

pub struct BlockContext<'function> {
    expressions: &'function mut ExprArena,
    locals: &'function mut Arena<Local>,
    scope: FastHashMap<Symbol, Handle<Local>>,
}

pub struct ParseResult {
    pub items: Vec<Item>,
    pub inference: Inference,
}

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    lookahead: Option<(Token, Span)>,

    errors: Vec<Error>,
    eof: bool,
    bad_symbol: Symbol,

    inference: Inference,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        let bad_symbol = lexer.extras.get_or_intern("Error");
        Parser {
            lexer,
            lookahead: None,
            errors: Vec::new(),
            eof: false,
            bad_symbol,
            inference: Inference::default(),
        }
    }

    fn next(&mut self) -> Option<(Token, Span)> {
        self.lookahead.take().or_else(|| {
            let token = self.lexer.next()?;
            let span = self.lexer.span().into();
            if let Token::Error = token {
                self.errors.push(
                    Error::custom(format!("Lexer error on: {}", self.lexer.slice()))
                        .with_span(span),
                )
            }
            Some((token, span))
        })
    }

    fn peek(&mut self) -> Option<&(Token, Span)> {
        if self.lookahead.is_none() {
            self.lookahead = self.next();
        }

        self.lookahead.as_ref()
    }

    fn expect_peek(&mut self) -> &(Token, Span) {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.bump());
        }

        self.lookahead.as_ref().unwrap()
    }

    fn bump(&mut self) -> (Token, Span) {
        match self.next() {
            Some(res) => res,
            None => {
                let span = self.lexer.span().into();
                if !self.eof {
                    self.errors.push(Error::eof(Span::from(span)));
                    self.eof = true;
                }

                (Token::Error, span)
            },
        }
    }

    fn expect(&mut self, expected: Token) -> Span {
        let (token, span) = self.bump();
        if token != expected && token != Token::Error {
            self.errors.push(Error::unexpected_token(token, span))
        }
        span
    }

    fn ident(&mut self) -> Ident {
        let (token, span) = self.bump();
        let symbol = match token {
            Token::Identifier(symbol) => symbol,
            Token::Error => self.bad_symbol,
            _ => {
                self.errors.push(Error::unexpected_token(token, span));
                self.bad_symbol
            },
        };
        Ident { symbol, span }
    }

    pub fn parse(mut self) -> Result<ParseResult, Vec<Error>> {
        let mut items = Vec::new();

        while let Some((token, span)) = self.next() {
            match token {
                Token::Entry => {
                    let ident = self.ident();
                    let mut expressions = ExprArena::default();
                    let mut locals = Arena::new();
                    let start = self.expect(Token::OpenCurlyBraces);
                    let mut ctx = BlockContext {
                        expressions: &mut expressions,
                        locals: &mut locals,
                        scope: FastHashMap::default(),
                    };
                    let block = self.block(&mut ctx, start);

                    items.push(Item::Entry(Entry {
                        ident,
                        block,
                        expressions,
                        locals,
                    }));
                },
                Token::Error => {},
                _ => {
                    self.errors.push(Error::unexpected_token(token, span));
                },
            }
        }

        self.inference.dump_errors(&mut self.errors);

        if self.errors.is_empty() {
            Ok(ParseResult {
                items,
                inference: self.inference,
            })
        } else {
            Err(self.errors)
        }
    }

    fn ty(&mut self) -> Handle<TypeData> {
        let (token, span) = self.bump();
        let ty = match token {
            Token::Int => TypeData::Int,
            Token::Float => TypeData::Float,
            Token::Bool => TypeData::Bool,
            Token::String => TypeData::String,
            Token::Error => TypeData::Error,
            _ => {
                self.errors.push(Error::unexpected_token(token, span));
                TypeData::Error
            },
        };

        self.inference.insert(ty, span)
    }

    fn block(&mut self, ctx: &mut BlockContext, mut span: Span) -> Block {
        let mut stmts = Arena::new();

        loop {
            match self.expect_peek().0 {
                Token::CloseCurlyBraces => {
                    let end_span = self.bump().1;
                    span = span.union(end_span);
                    break;
                },
                Token::Let => {
                    let start_span = self.bump().1;

                    let ident = self.ident();
                    let ty = if let Token::Colon = self.expect_peek().0 {
                        self.bump();
                        self.ty()
                    } else {
                        self.inference.insert(TypeData::Top, ident.span)
                    };

                    let init = if let Some(&(Token::Equal, _)) = self.peek() {
                        self.bump();
                        let expr = self.expression(ctx, 0);
                        Some(expr)
                    } else {
                        None
                    };

                    let end_span = self.expect(Token::SemiColon);
                    let span = start_span.union(end_span);

                    let local = ctx.locals.append(
                        Local {
                            ident,
                            ty,
                            init: init.is_some(),
                        },
                        span,
                    );
                    ctx.scope.insert(ident.symbol, local);

                    if let Some(init) = init {
                        self.inference.unify(ty, ctx.expressions.get_type(init));
                        stmts.append(Stmt::LocalInit(local, init), span);
                    }
                },
                _ => {
                    let expr = self.expression(ctx, 0);
                    let span = ctx.expressions.get_span(expr);
                    self.expect(Token::SemiColon);
                    stmts.append(Stmt::Expr(expr), span);
                },
            }
        }

        Block { stmts, span }
    }

    fn atom_expression(&mut self, ctx: &mut BlockContext) -> Handle<Expr> {
        let (token, span) = self.bump();
        let (expr, ty) = match token {
            Token::Identifier(symbol) => {
                let handle = match ctx.scope.get(&symbol) {
                    Some(&handle) => handle,
                    None => {
                        self.errors
                            .push(Error::new(ErrorKind::UnknownVariable(symbol)).with_span(span));

                        let ty = self.inference.insert(TypeData::Top, span);
                        ctx.locals.append(
                            Local {
                                ident: Ident { symbol, span },
                                ty,
                                init: false,
                            },
                            span,
                        )
                    },
                };

                (Expr::Variable(handle), ctx.locals[handle].ty)
            },
            Token::FloatLiteral(value) => {
                let float = self.inference.insert(TypeData::Float, span);
                (Expr::Literal(Literal::Float(value)), float)
            },
            Token::IntLiteral(value) => {
                let int = self.inference.insert(TypeData::Int, span);
                (Expr::Literal(Literal::Int(value)), int)
            },
            Token::BoolLiteral(value) => {
                let boolean = self.inference.insert(TypeData::Bool, span);
                (Expr::Literal(Literal::Boolean(value)), boolean)
            },
            Token::StringLiteral(value) => {
                let string = self.inference.insert(TypeData::String, span);
                (Expr::Literal(Literal::String(value)), string)
            },
            Token::OpenParentheses => {
                let expr = self.expression(ctx, 0);
                let ty = ctx.expressions.get_type(expr);
                (Expr::Parenthesized(expr), ty)
            },
            Token::In => {
                let unit = self.inference.insert(TypeData::Unit, Span::None);
                let ty = self
                    .inference
                    .insert(TypeData::Fn(unit, vec![], true), span);
                (Expr::Intrisinc(Intrisinc::In), ty)
            },
            Token::Out => {
                let unit = self.inference.insert(TypeData::Unit, Span::None);
                let ty = self
                    .inference
                    .insert(TypeData::Fn(unit, vec![], true), span);
                (Expr::Intrisinc(Intrisinc::Out), ty)
            },
            Token::Error => {
                let ty = self.inference.insert(TypeData::Error, span);
                (Expr::Error, ty)
            },
            _ => {
                let ty = self.inference.insert(TypeData::Error, span);
                self.errors.push(Error::unexpected_token(token, span));
                (Expr::Error, ty)
            },
        };

        ctx.expressions.append(expr, span, ty)
    }

    fn prefix_expression(&mut self, ctx: &mut BlockContext) -> Handle<Expr> {
        let mut stack = Vec::new();

        loop {
            let op = match self.expect_peek().0 {
                Token::Bang => UnaryOp::BitWiseNot,
                Token::Minus => UnaryOp::Negation,
                _ => break,
            };

            let span = self.bump().1;

            stack.push((op, span))
        }

        let mut tgt = self.atom_expression(ctx);
        let mut span = ctx.expressions.get_span(tgt);

        while let Some((op, op_span)) = stack.pop() {
            let tgt_ty = ctx.expressions.get_type(tgt);
            let ty = self.inference.unary_op(tgt_ty, op, span);
            span = span.union(op_span);
            tgt = ctx.expressions.append(Expr::UnaryOp { tgt, op }, span, ty);
        }

        tgt
    }

    fn expression(&mut self, ctx: &mut BlockContext, min_bp: u8) -> Handle<Expr> {
        let mut lhs = self.prefix_expression(ctx);
        let mut span = ctx.expressions.get_span(lhs);

        loop {
            match self.expect_peek().0 {
                Token::OpenParentheses => {
                    self.bump();

                    let mut args = Vec::new();
                    loop {
                        args.push(match self.expect_peek().0 {
                            Token::CloseParentheses => break,
                            _ => self.expression(ctx, 0),
                        });

                        if let Token::Comma = self.expect_peek().0 {
                            self.bump();
                            continue;
                        }

                        break;
                    }
                    let end_span = self.expect(Token::CloseParentheses);

                    let span = span.union(end_span);
                    let fun_ty = ctx.expressions.get_type(lhs);
                    let ty = self.inference.call(
                        fun_ty,
                        &args
                            .iter()
                            .map(|&handle| ctx.expressions.get_type(handle))
                            .collect::<Vec<_>>(),
                        span,
                    );

                    lhs = ctx
                        .expressions
                        .append(Expr::Call { fun: lhs, args }, span, ty)
                },
                ref token => {
                    if let Some((l_bp, r_bp)) = infix_binding_power(token) {
                        if l_bp < min_bp {
                            break;
                        }

                        let token = self.bump().0;
                        let rhs = self.expression(ctx, r_bp);
                        let end_span = ctx.expressions.get_span(rhs);
                        span = span.union(end_span);

                        let lhs_ty = ctx.expressions.get_type(lhs);
                        let rhs_ty = ctx.expressions.get_type(rhs);
                        let (expr, ty) = if let Token::Equal = token {
                            self.inference.unify(lhs_ty, rhs_ty);
                            (Expr::Assignment { lhs, rhs }, rhs_ty)
                        } else {
                            let op = token_to_bin_op(token).unwrap();
                            let ty = self.inference.binary_op(lhs_ty, rhs_ty, op, span);
                            (Expr::BinaryOp { lhs, op, rhs }, ty)
                        };

                        lhs = ctx.expressions.append(expr, span, ty);

                        continue;
                    }
                },
            }

            break;
        }

        lhs
    }
}

fn token_to_bin_op(token: Token) -> Option<BinaryOp> {
    Some(match token {
        Token::LogicalOr => BinaryOp::LogicalOr,
        Token::LogicalAnd => BinaryOp::LogicalAnd,
        Token::Equality => BinaryOp::Equality,
        Token::Inequality => BinaryOp::Inequality,
        Token::Greater => BinaryOp::Greater,
        Token::GreaterEqual => BinaryOp::GreaterEqual,
        Token::Less => BinaryOp::Less,
        Token::LessEqual => BinaryOp::LessEqual,
        Token::BitWiseOr => BinaryOp::BitWiseOr,
        Token::BitWiseXor => BinaryOp::BitWiseXor,
        Token::BitWiseAnd => BinaryOp::BitWiseAnd,
        Token::Plus => BinaryOp::Addition,
        Token::Minus => BinaryOp::Subtraction,
        Token::Star => BinaryOp::Multiplication,
        Token::Slash => BinaryOp::Division,
        Token::Percent => BinaryOp::Remainder,
        _ => return None,
    })
}

fn infix_binding_power(token: &Token) -> Option<(u8, u8)> {
    Some(match *token {
        Token::Equal => (2, 1),
        Token::LogicalOr => (3, 4),
        Token::LogicalAnd => (5, 6),
        Token::Equality
        | Token::Inequality
        | Token::Greater
        | Token::GreaterEqual
        | Token::Less
        | Token::LessEqual => (7, 8),
        Token::BitWiseOr => (9, 10),
        Token::BitWiseXor => (11, 12),
        Token::BitWiseAnd => (13, 14),
        Token::Plus | Token::Minus => (17, 18),
        Token::Star | Token::Slash | Token::Percent => (19, 20),
        _ => return None,
    })
}
