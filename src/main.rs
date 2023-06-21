use std::collections::HashMap;

use crate::my_lang::Scope;

mod my_lang;

fn run_code_string(str: &str) {
    use my_lang::{execute, parse, tokenize};

    let tokens = tokenize(str);
    let ast = parse(&tokens.into_boxed_slice()).0;
    execute(&ast, &mut Scope::new(None, HashMap::new()));
}

fn main() {
    run_code_string(
        "(
                (let a = ((42 + 69) * 1337))
                (let b = 148407)

                (if (a == b) (
                    (print 12345)
                ) else (
                    (print 54321)
                ))

                (for i in 10 (
                    (print i)
                ))
            )",
    )
}
