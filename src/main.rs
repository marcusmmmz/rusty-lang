mod my_lang;
mod my_lisp;

fn main() {
    // most actual code is in the language's tests

    my_lang::run_code_string("let a = 10;");

    my_lisp::run_code_string(
        "(
            (print \"Hello world\")
        )",
    );
}
