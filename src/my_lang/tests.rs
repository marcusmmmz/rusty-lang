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
fn function_declaration() {
    assert_eq!(
        "function f() {
		
	}"
        .replace("\t", ""),
        code_string_to_js("fn f() {}")
    );
}

#[test]
fn conditional() {
    assert_eq!(
        "if (true) {
			let x = 10
		}"
        .replace("\t", ""),
        code_string_to_js("if true { let x = 10 }")
    );
}

#[test]
fn while_loop() {
    assert_eq!(
        "while (true) {
			console.log(10)
		}"
        .replace("\t", ""),
        code_string_to_js("while true { print(10) }")
    );
}

#[test]
fn asd() {
    assert_eq!(
        "function dostuff() {
			let i = 0;
			while (false) {
				i = 1
			};
			if (true) {
				console.log(i)
			}
		};
		dostuff()"
            .replace("\t", ""),
        code_string_to_js(
            "
				fn dostuff() {
					let i = 0
					while false {
						i = 1
					}
	
					if true { print(i) }
				}

				dostuff()
		  "
        )
    );
}
