use crate::my_lang::code_string_to_js;
use std::{error::Error, fs, process};

pub fn execute(path: &str) -> Result<(String, String), Box<dyn Error>> {
	let code_string = fs::read_to_string(path)?;

	let js = code_string_to_js(&code_string);

	let res = process::Command::new("node")
		.args(["-e", &format!("{js}")])
		.output()?;

	return Ok((
		String::from_utf8_lossy(&res.stdout).to_string(),
		String::from_utf8_lossy(&res.stderr).to_string(),
	));
}

#[test]
fn inception() {
	let res = execute("./src/inception_lang/mod.test").unwrap();

	println!("{}{}", res.0, res.1);
}
