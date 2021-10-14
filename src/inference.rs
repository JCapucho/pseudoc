use crate::common::{
    arena::{Arena, Handle},
    error::Error,
    src::Span,
    BinaryOp, PrimitiveType, Ty, UnaryOp,
};

#[derive(Debug, PartialEq)]
pub enum TypeData {
    Top,
    Ref(Handle<TypeData>),

    Fn(Handle<TypeData>, Vec<Handle<TypeData>>, bool),
    Number,
    Int,
    Float,
    Bool,
    String,
    Unit,

    Error,
}

#[derive(Debug, Default)]
pub struct Inference {
    data: Arena<TypeData>,
    errors: Vec<Error>,
}

impl Inference {
    pub fn insert(&mut self, ty: TypeData, span: Span) -> Handle<TypeData> {
        self.data.append(ty, span)
    }

    pub fn link(&mut self, source: Handle<TypeData>, target: Handle<TypeData>) {
        self.data[target] = TypeData::Ref(source)
    }

    pub fn unify(&mut self, a: Handle<TypeData>, b: Handle<TypeData>) {
        match (&self.data[a], &self.data[b]) {
            (&TypeData::Ref(a), _) => self.unify(a, b),
            (_, &TypeData::Ref(b)) => self.unify(a, b),
            (TypeData::Top, _) => self.link(b, a),
            (_, TypeData::Top) => self.link(a, b),
            (TypeData::Number, TypeData::Int) | (TypeData::Number, TypeData::Float) => {
                self.link(b, a)
            },
            (TypeData::Int, TypeData::Number) | (TypeData::Float, TypeData::Number) => {
                self.link(a, b)
            },
            (a, b) if a == b => {},
            (_, TypeData::Error) | (TypeData::Error, _) => {
                self.data[a] = TypeData::Error;
                self.data[b] = TypeData::Error;
            },
            _ => {
                self.data[a] = TypeData::Error;
                self.data[b] = TypeData::Error;

                self.errors.push(
                    Error::custom(format!(
                        "Types {:?} and {:?} don't match up",
                        self.data[a], self.data[b]
                    ))
                    .with_span(self.data.get_span(a))
                    .with_span(self.data.get_span(b)),
                )
            },
        }
    }

    #[must_use]
    pub fn unary_op(&mut self, a: Handle<TypeData>, op: UnaryOp, span: Span) -> Handle<TypeData> {
        let ty = match op {
            UnaryOp::BitWiseNot => self.insert(TypeData::Int, span),
            UnaryOp::Negation => self.insert(TypeData::Number, span),
        };

        self.unify(a, ty);
        ty
    }

    #[must_use]
    pub fn binary_op(
        &mut self,
        a: Handle<TypeData>,
        b: Handle<TypeData>,
        op: BinaryOp,
        span: Span,
    ) -> Handle<TypeData> {
        match op {
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                let boolean = self.insert(TypeData::Bool, span);
                self.unify(boolean, a);
                self.unify(a, b);
                boolean
            },
            BinaryOp::Inequality | BinaryOp::Equality => {
                let ty = self.insert(TypeData::Top, span);
                self.unify(a, b);
                self.unify(a, ty);
                ty
            },
            BinaryOp::Addition
            | BinaryOp::Subtraction
            | BinaryOp::Multiplication
            | BinaryOp::Division => {
                let num = self.insert(TypeData::Number, span);
                self.unify(a, b);
                self.unify(num, a);
                num
            },
            BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::Less | BinaryOp::LessEqual => {
                let num = self.insert(TypeData::Number, Span::None);
                self.unify(a, b);
                self.unify(num, a);
                self.insert(TypeData::Bool, span)
            },
            BinaryOp::Remainder
            | BinaryOp::BitWiseOr
            | BinaryOp::BitWiseXor
            | BinaryOp::BitWiseAnd => {
                let int = self.insert(TypeData::Int, span);
                self.unify(int, a);
                self.unify(a, b);
                int
            },
        }
    }

    #[must_use]
    fn get(&mut self, handle: Handle<TypeData>) -> Handle<TypeData> {
        let mut base = handle;
        loop {
            match self.data[base] {
                TypeData::Ref(inner) => base = inner,
                _ => break,
            }
        }
        base
    }

    #[must_use]
    pub fn call(
        &mut self,
        fun: Handle<TypeData>,
        args: &[Handle<TypeData>],
        span: Span,
    ) -> Handle<TypeData> {
        let base = self.get(fun);
        let (res, def_args, varargs) = match self.data[fun] {
            TypeData::Fn(res, ref args, varargs) => (res, args.clone(), varargs),
            TypeData::Error => (fun, vec![], true),
            _ => {
                let res = self.insert(TypeData::Error, Span::None);
                self.errors.push(
                    Error::custom(format!("{:#?} isn't a function", self.data[base]))
                        .with_span(self.data.get_span(fun)),
                );
                (res, vec![], true)
            },
        };

        if args.len() != def_args.len() && !varargs {
            self.errors.push(
                Error::custom(format!(
                    "The function takes {} arguments, but {} where provided",
                    def_args.len(),
                    args.len()
                ))
                .with_span(span),
            );
        }

        for (&call, def) in args.iter().zip(def_args.into_iter()) {
            self.unify(call, def)
        }

        let ty = self.insert(TypeData::Top, span);
        self.link(res, ty);
        ty
    }

    pub fn dump_errors(&mut self, sink: &mut Vec<Error>) {
        sink.reserve(self.errors.len());
        sink.append(&mut self.errors);
        self.errors.clear();
    }

    pub fn realize(&self, ty: Handle<TypeData>) -> Result<Ty, Error> {
        debug_assert!(self.errors.is_empty());
        match self.data[ty] {
            TypeData::Number | TypeData::Top => Err(Error::custom(String::from(
                "Not enough information to infer type",
            ))
            .with_span(self.data.get_span(ty))),
            TypeData::Ref(inner) => self.realize(inner),
            TypeData::Int => Ok(Ty::Primitive(PrimitiveType::Int)),
            TypeData::Float => Ok(Ty::Primitive(PrimitiveType::Float)),
            TypeData::Bool => Ok(Ty::Primitive(PrimitiveType::Bool)),
            TypeData::String => Ok(Ty::Primitive(PrimitiveType::String)),
            TypeData::Unit => Err(
                Error::custom(String::from("Expression doesn't return a value"))
                    .with_span(self.data.get_span(ty)),
            ),
            TypeData::Error => unreachable!(),
            TypeData::Fn(_, _, _) => Err(Error::custom(String::from(
                "Function can't be used as a type",
            ))
            .with_span(self.data.get_span(ty))),
        }
    }
}
