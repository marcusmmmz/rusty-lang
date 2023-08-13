use crate::my_lang::code_string_to_js;
use std::{error::Error, fs, process};

pub fn execute_javascript(
	js: &str,
) -> Result<(String, String), Box<dyn Error>> {
	let res = process::Command::new("node")
		.args(["-e", &format!("{js}")])
		.output()?;

	return Ok((
		String::from_utf8_lossy(&res.stdout).to_string(),
		String::from_utf8_lossy(&res.stderr).to_string(),
	));
}

fn compile_using_rust_compiler() -> String {
	let code_to_compile =
		fs::read_to_string("./src/inception_lang/mod.test").unwrap();

	let output_js_code = code_string_to_js(&code_to_compile);

	return output_js_code;
}

fn compile_using_inception_compiler(compiler_code_in_js: &str) -> String {
	let code_to_compile =
		fs::read_to_string("./src/inception_lang/mod.test").unwrap();

	let mut compiler_with_injected_code = String::from(compiler_code_in_js);

	let code_to_inject = code_to_compile.replace("`", "\\`");

	compiler_with_injected_code.push_str(&format!(
		"\nconsole.log(
				code_string_to_js(String.raw`{code_to_inject}`)
			)"
	));

	fs::write("./src/inception_lang/test.js", &compiler_with_injected_code)
		.unwrap();

	let outputted_compiler_in_js =
		execute_javascript(&compiler_with_injected_code).unwrap();

	return format!(
		"{}{}",
		outputted_compiler_in_js.0, outputted_compiler_in_js.1
	);
}

#[test]
fn compile_recursively() {
	let first_compiler = compile_using_rust_compiler();
	fs::write("./src/inception_lang/first.js", &first_compiler).unwrap();

	let mut final_compiler = first_compiler;

	for _ in 0..10 {
		final_compiler = compile_using_inception_compiler(&final_compiler)
	}

	fs::write("./src/inception_lang/final.js", &final_compiler).unwrap();
}
