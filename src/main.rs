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

/// Translate pseudoc
#[derive(FromArgs)]
struct Arguments {
    /// pseudoc file to translate
    #[argh(positional)]
    input: String,
    /// pseudoc file to output to, if none is provided stdout is used
    #[argh(positional)]
    output: Option<String>,
}

fn main() {
    let args: Arguments = argh::from_env();
    let input = std::fs::read_to_string(&args.input).expect("Failed to read the input file");

    let mut files = SimpleFiles::new();

    let file_id = files.add(args.input, &input);

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

    let mut stdout = std::io::stdout();
    let mut maybe_file = args.output.map(|path| {
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

    let mut backend = backend::PseudoBackend::new(&parse_data, &resolver, sink);
    match backend.emit() {
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
    };
}
