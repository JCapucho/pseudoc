mod backend;
mod common;
mod inference;
mod lexer;
mod parser;

use common::{error::Error, Rodeo};
use lasso::RodeoResolver;

pub struct ParseResult {
    pub(crate) name: Option<common::Ident>,
    pub(crate) main_block: common::ast::MainBlock,
    pub(crate) items: Vec<common::ast::Item>,
    pub(crate) inference: inference::Inference,
}

pub fn new_rodeo() -> Rodeo { Rodeo::with_hasher(Default::default()) }

pub fn build_parse(source: &str, rodeo: &mut Rodeo) -> Result<ParseResult, Vec<Error>> {
    let lexer = lexer::Lexer::with_extras(source, rodeo);
    parser::Parser::new(lexer).parse()
}

pub fn build_dot(
    parse: &ParseResult,
    resolver: &RodeoResolver,
    sink: &mut dyn std::io::Write,
) -> Result<(), Vec<Error>> {
    let mut backend = backend::dot::DotBackend::new(parse, resolver, sink);
    backend.emit().map_err(|e| vec![e])
}

pub fn build_pseudo(
    parse: &ParseResult,
    resolver: &RodeoResolver,
    default_name: &str,
    sink: &mut dyn std::io::Write,
) -> Result<(), Vec<Error>> {
    let config = backend::Config::default();
    let mut backend = backend::pseudo::PseudoBackend::new(&parse, resolver, &config, sink);
    backend.emit(&default_name).map_err(|e| vec![e])
}

pub fn build_pascal(
    parse: &ParseResult,
    resolver: &RodeoResolver,
    default_name: &str,
    sink: &mut dyn std::io::Write,
) -> Result<(), Vec<Error>> {
    let config = backend::Config::pascal();
    let mut backend = backend::pseudo::PseudoBackend::new(&parse, resolver, &config, sink);
    backend.emit(&default_name).map_err(|e| vec![e])
}
