use std::collections::HashMap;

use crate::my_lang::Scope;

mod my_lang;

fn run_code_string(str: &str) {
    use my_lang::{execute, parse, tokenize};

    let tokens = tokenize(str);
    let ast = parse(&tokens.into_boxed_slice());
    execute(&ast, &mut Scope::new(None, HashMap::new()));
}

fn main() {
    run_code_string(
        "(
                (fn factorial (n) (
                    (if (n == 1) (
                        n
                    ) else (
                        n * (factorial (n - 1))
                    ))
                ))

                (for i in 10 (
                    (print (factorial (i + 1)))
                ))

                (let arr = [1 2 3])
                (print arr)
                (let arr = (arr + 4))
                (print arr)
                (print 2)
            )",
    );
}
