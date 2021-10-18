use std::fs::read_to_string;

#[test]
fn insta_test() {
    insta::glob!("*.psc", |file| {
        let code = read_to_string(file).unwrap();

        let mut rodeo = pseudoc::new_rodeo();
        let parse_result = pseudoc::build_parse(&code, &mut rodeo);
        let resolver = rodeo.into_resolver();

        let default_name = "test";

        match parse_result {
            Ok(ref parse) => {
                let mut buf = Vec::new();

                pseudoc::build_pseudo(parse, &resolver, default_name, &mut buf).unwrap();
                buf.push(b'\n');
                pseudoc::build_pascal(parse, &resolver, default_name, &mut buf).unwrap();
                buf.push(b'\n');
                pseudoc::build_dot(parse, &resolver, &mut buf).unwrap();
                buf.push(b'\n');
                let out = std::str::from_utf8(buf.as_slice()).unwrap().to_string();
                insta::assert_snapshot!(out);
            },
            Err(errors) => {
                insta::assert_debug_snapshot!(errors);
            },
        }
    });
}
