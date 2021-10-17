mod backend;
mod common;
mod inference;
mod lexer;
mod parser;

use argh::FromArgs;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use std::path::PathBuf;

/// Translate pseudoc
#[derive(FromArgs)]
struct Arguments {
    /// pseudoc file to translate
    #[argh(positional)]
    input: PathBuf,
    /// pseudoc file to output to, if none is provided stdout is used.
    ///
    /// The type to output is guessed from the file extension otherwise
    /// pseudo is used as a default
    #[argh(positional)]
    output: Option<PathBuf>,
}

fn main() {
    let args: Arguments = argh::from_env();
    let input = std::fs::read_to_string(&args.input).expect("Failed to read the input file");

    let mut files = SimpleFiles::new();

    let file_id = files.add(args.input.to_string_lossy(), &input);

    let mut rodeo = common::Rodeo::with_hasher(Default::default());
    let lexer = lexer::Lexer::with_extras(&input, &mut rodeo);
    let parse = parser::Parser::new(lexer).parse();

    let resolver = rodeo.into_resolver();

    let parse_data = match parse {
        Ok(items) => items,
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            for err in errors {
                term::emit(
                    &mut writer.lock(),
                    &config,
                    &files,
                    &err.codespan_diagnostic(file_id, &resolver),
                )
                .expect("Failed to output error");
            }
            return;
        },
    };

    let default_name = args
        .input
        .file_stem()
        .map(|val| val.to_string_lossy())
        .or_else(|| args.input.file_name().map(|val| val.to_string_lossy()))
        .unwrap_or_else(|| args.input.to_string_lossy());

    #[derive(Debug, PartialEq, Eq)]
    enum Backend {
        Pseudo,
        Pascal,
        Dot,
    }

    let backend = args
        .output
        .as_ref()
        .and_then(|f| f.extension())
        .map(|val| match &*val.to_string_lossy() {
            "dot" => Backend::Dot,
            "pas" => Backend::Pascal,
            _ => Backend::Pseudo,
        })
        .unwrap_or(Backend::Pseudo);

    let mut stdout = std::io::stdout();
    let mut maybe_file = args.output.as_ref().map(|path| {
        std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
            .expect("Failed to open output file")
    });
    let sink: &mut dyn std::io::Write = match maybe_file {
        Some(ref mut file) => file,
        None => &mut stdout,
    };

    let result = match backend {
        Backend::Pseudo => {
            let config = backend::Config::default();
            let mut backend =
                backend::pseudo::PseudoBackend::new(&parse_data, &resolver, &config, sink);
            backend.emit(&default_name)
        },
        Backend::Pascal => {
            let config = backend::Config::pascal();
            let mut backend =
                backend::pseudo::PseudoBackend::new(&parse_data, &resolver, &config, sink);
            backend.emit(&default_name)
        },
        Backend::Dot => {
            let mut backend = backend::dot::DotBackend::new(&parse_data, &resolver, sink);
            backend.emit()
        },
    };

    match result {
        Ok(_) => {},
        Err(err) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &err.codespan_diagnostic(file_id, &resolver),
            )
            .expect("Failed to output error");
        },
    }
}
