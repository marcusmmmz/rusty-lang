use crate::my_lang::run_code_string;

mod my_lang;
mod tests;

fn main() {
    // Right now most of the code is in src/tests

    run_code_string(
        "(
                (print \"Hello world\")
            )",
    );
}
