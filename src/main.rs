mod lisp;

fn run_code_string(str: &str) {
    use lisp::{execute, parse, tokenize};
    let tokens = tokenize(str);
    let ast = parse(&tokens.into_boxed_slice()).0;
    // dbg!(ast);
    execute(&ast);
}

fn main() {
    run_code_string("(print (+ (* 42 69) 1337))");
    run_code_string("(print (* (+ 42 69) 1337))");
}
