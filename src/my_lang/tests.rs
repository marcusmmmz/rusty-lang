#[cfg(test)]
use crate::my_lang::code_string_to_js;

#[test]
fn hello_world() {
	assert_eq!(
		"console.log(\"Hello World\")",
		code_string_to_js("print(\"Hello World\")")
	);
}

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
fn comparison_operators() {
	assert_eq!(
		"if (1 == 1) {
			
		}"
		.replace("\t", ""),
		code_string_to_js("if 1 == 1 {}")
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
fn operator_precedence() {
	assert_eq!(
		"console.log(1 + 3 == 2 + 2)".replace("\t", ""),
		code_string_to_js("print(1 + 3 == 2 + 2)")
	);
}

#[test]
fn array_creation() {
	assert_eq!(
		"console.log([1, 2, 3])".replace("\t", ""),
		code_string_to_js("print([1, 2, 3])")
	);
}

#[test]
fn member_expression() {
	assert_eq!(
		"console.log(a.b.c.d)".replace("\t", ""),
		code_string_to_js("print(a.b.c.d)")
	);
}

#[test]
fn fibonnaci() {
	assert_eq!(
		"function factorial(n) {
			if (n == 1) {\nreturn n\n};
			return n * factorial(n - 1)
		};
		factorial(10)"
			.replace("\t", ""),
		code_string_to_js(
			"fn factorial(n) {
				if n == 1 {
					return n
				}
				
				return n * factorial(n - 1)
			}
			
			factorial(10)
			"
		)
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
