#[cfg(test)]
use crate::my_lang::run_code_string;

#[test]
fn let_declaration() {
    run_code_string("let a = 10;");
}

#[test]
fn function_call() {
    run_code_string("print(1);");
}
