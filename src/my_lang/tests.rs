#[cfg(test)]
use crate::my_lang::code_string_to_js;

#[test]
fn let_declaration() {
    assert_eq!("let a = 10", code_string_to_js("let a = 10;"));
}

#[test]
fn assignment() {
    assert_eq!("a = 10", code_string_to_js("a = 10;"));
}

#[test]
fn function_call() {
    assert_eq!("console.log(1)", code_string_to_js("print(1);"));
}

#[test]
fn conditional() {
    assert_eq!(
        "if (true) {let x = 10}",
        code_string_to_js("if true { let x = 10 }")
    );
}

#[test]
fn while_loop() {
    assert_eq!(
        "while (true) {let x = 10}",
        code_string_to_js("while true { let x = 10 }")
    );
}
