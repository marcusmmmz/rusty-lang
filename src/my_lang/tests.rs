#[cfg(test)]
use crate::my_lang::code_string_to_js;

#[test]
fn let_declaration() {
    assert_eq!("let a = 10", code_string_to_js("let a = 10;"));
}

#[test]
fn function_call() {
    assert_eq!("console.log(1)", code_string_to_js("print(1);"));
}
