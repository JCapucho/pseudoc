use crate::common::{PrimitiveType, Rodeo, RodeoResolver, Symbol};
use logos::{Lexer as LogosLexer, Logos};
use std::fmt;

pub type Lexer<'source> = LogosLexer<'source, Token>;

fn ident(lex: &mut LogosLexer<Token>) -> Option<Symbol> {
    let slice = lex.slice();

    Some(lex.extras.get_or_intern(&slice[..slice.len()]))
}

fn boolean(lex: &mut LogosLexer<Token>) -> Option<bool> {
    let slice = lex.slice();

    match &slice[..slice.len()] {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Logos)]
#[logos(extras = &'s mut Rodeo)]
pub enum Token {
    #[regex(r"\p{XID_Start}\p{XID_Continue}*", ident)]
    Identifier(Symbol),

    #[token("(")]
    OpenParentheses,
    #[token("{")]
    OpenCurlyBraces,
    #[token("[")]
    OpenSquareBrackets,

    #[token(")")]
    CloseParentheses,
    #[token("}")]
    CloseCurlyBraces,
    #[token("]")]
    CloseSquareBrackets,

    #[regex("[-+]?[0-9]+\\.[0-9]*", |lex| lex.slice().parse().ok())]
    FloatLiteral(f64),
    #[regex("[-+]?[0-9]+", |lex| lex.slice().parse().ok())]
    IntLiteral(i64),
    #[regex("(true|false)", boolean)]
    BoolLiteral(bool),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice().to_string())]
    StringLiteral(String),

    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("bool")]
    Bool,
    #[token("string")]
    String,

    #[token("entry")]
    Entry,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token(":")]
    Colon,
    #[token("=")]
    Equal,
    #[token("->")]
    Arrow,
    #[token(",")]
    Comma,
    #[token(";")]
    SemiColon,
    #[token(".")]
    Dot,

    #[token("||")]
    LogicalOr,
    #[token("&&")]
    LogicalAnd,

    #[token("!=")]
    Inequality,
    #[token("==")]
    Equality,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    #[token("|")]
    BitWiseOr,
    #[token("^")]
    BitWiseXor,
    #[token("&")]
    BitWiseAnd,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("!")]
    Bang,
    #[token("%")]
    Percent,

    // FIXME: intrisinc should be IR not syntax
    #[token("In")]
    In,
    #[token("Out")]
    Out,

    #[error]
    #[regex(r"[ \r\t\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*\n?", logos::skip)]
    // TODO: Nested multi-line comments
    #[regex(r"/\*(?:[^*]|\*[^/])*\*/", logos::skip)]
    Error,
}

impl Token {
    pub fn display<'a>(&'a self, rodeo: &'a RodeoResolver) -> impl fmt::Display + 'a {
        struct TokenDisplay<'a> {
            tok: &'a Token,
            rodeo: &'a RodeoResolver,
        }

        impl<'a> fmt::Display for TokenDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.tok {
                    Token::Identifier(ident) => write!(f, "{}", self.rodeo.resolve(ident)),

                    Token::OpenParentheses => write!(f, "("),
                    Token::OpenCurlyBraces => write!(f, "{{"),
                    Token::OpenSquareBrackets => write!(f, "["),
                    Token::CloseParentheses => write!(f, ")"),
                    Token::CloseCurlyBraces => write!(f, "}}"),
                    Token::CloseSquareBrackets => write!(f, "]"),

                    Token::BoolLiteral(bool) => write!(f, "{}", bool),
                    Token::IntLiteral(int) => write!(f, "{}", int),
                    Token::FloatLiteral(float) => write!(f, "{}", float),
                    Token::StringLiteral(value) => write!(f, "{}", value),

                    Token::Float => write!(f, "float"),
                    Token::Int => write!(f, "int"),
                    Token::Bool => write!(f, "bool"),
                    Token::String => write!(f, "string"),

                    Token::Entry => write!(f, "entry"),
                    Token::Const => write!(f, "const"),
                    Token::Fn => write!(f, "fn"),
                    Token::Return => write!(f, "return"),
                    Token::If => write!(f, "if"),
                    Token::Else => write!(f, "else"),
                    Token::Let => write!(f, "let"),

                    Token::Colon => write!(f, ":"),
                    Token::Equal => write!(f, "="),
                    Token::Arrow => write!(f, "->"),
                    Token::Comma => write!(f, ","),
                    Token::SemiColon => write!(f, ";"),
                    Token::Dot => write!(f, "."),

                    Token::LogicalOr => write!(f, "||"),
                    Token::LogicalAnd => write!(f, "&&"),

                    Token::Inequality => write!(f, "!="),
                    Token::Equality => write!(f, "=="),
                    Token::Greater => write!(f, ">"),
                    Token::GreaterEqual => write!(f, ">="),
                    Token::Less => write!(f, "<"),
                    Token::LessEqual => write!(f, "<="),

                    Token::BitWiseOr => write!(f, "|"),
                    Token::BitWiseXor => write!(f, "^"),
                    Token::BitWiseAnd => write!(f, "&"),

                    Token::Plus => write!(f, "+"),
                    Token::Minus => write!(f, "-"),
                    Token::Slash => write!(f, "/"),
                    Token::Star => write!(f, "*"),
                    Token::Bang => write!(f, "!"),
                    Token::Percent => write!(f, "%"),

                    Token::In => write!(f, "In"),
                    Token::Out => write!(f, "Out"),

                    Token::Error => write!(f, "Error"),
                }
            }
        }

        TokenDisplay { tok: self, rodeo }
    }
}
