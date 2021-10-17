pub mod dot;
pub mod pseudo;

const IDENTATION: &str = "    ";

pub struct Config<'config> {
    pub pascal: bool,

    pub program: &'config str,
    pub variable: &'config str,
    pub begin: &'config str,
    pub end: &'config str,

    pub assign: &'config str,

    pub if_kw: &'config str,
    pub then_kw: &'config str,
    pub else_kw: &'config str,
    pub endif_kw: &'config str,

    pub int: &'config str,
    pub string: &'config str,
    pub float: &'config str,
    pub boolean: &'config str,

    pub input: &'config str,
    pub output: &'config str,
}

impl<'config> Config<'config> {
    pub fn pascal() -> Self {
        Config {
            pascal: true,

            program: "Program",
            variable: "Var",
            begin: "Begin",
            end: "End",

            assign: ":=",

            if_kw: "If",
            then_kw: "Then",
            else_kw: "Else",
            endif_kw: "EndIf",

            int: "integer",
            string: "string",
            float: "real",
            boolean: "boolean",

            input: "Readln",
            output: "Writeln",
        }
    }
}

impl<'config> Default for Config<'config> {
    fn default() -> Self {
        Config {
            pascal: false,

            program: "Algoritmo",
            variable: "Variável",
            begin: "Início",
            end: "Fim",

            assign: "🠔",

            if_kw: "Se",
            then_kw: "Então",
            else_kw: "Senão",
            endif_kw: "FimSe",

            int: "inteiro",
            string: "texto",
            float: "real",
            boolean: "booleano",

            input: "Ler",
            output: "Escrever",
        }
    }
}
