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

#[derive(Debug, PartialEq, Eq)]
enum Backend {
    Pseudo,
    Pascal,
    Dot,
}

fn main() {
    let args: Arguments = argh::from_env();
    let input = std::fs::read_to_string(&args.input).expect("Failed to read the input file");

    let mut files = SimpleFiles::new();

    let file_id = files.add(args.input.to_string_lossy(), &input);

    let default_name = args
        .input
        .file_stem()
        .map(|val| val.to_string_lossy())
        .or_else(|| args.input.file_name().map(|val| val.to_string_lossy()))
        .unwrap_or_else(|| args.input.to_string_lossy());

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

    let mut rodeo = pseudoc::new_rodeo();
    let parse_result = pseudoc::build_parse(&input, &mut rodeo);
    let resolver = rodeo.into_resolver();
    let result = parse_result.and_then(|parse| match backend {
        Backend::Pseudo => pseudoc::build_pseudo(&parse, &resolver, &default_name, sink),
        Backend::Pascal => pseudoc::build_pascal(&parse, &resolver, &default_name, sink),
        Backend::Dot => pseudoc::build_dot(&parse, &resolver, sink),
    });

    if let Err(errors) = result {
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
    }
}
