#[cfg(test)]
use super::my_lang::{run_code_string, Value};

#[test]
#[should_panic]
fn variable_outside_scope() {
    run_code_string(
        "(
			(
				(let x = 1)
			)
			(x = 1336)
		)",
    );
}

#[test]
#[should_panic]
fn variable_outside_function_scope() {
    run_code_string(
        "(
			(fn f () (
				x
			))

			(
				(let x = 1337)
				(f ())
			)
		)",
    );
}

#[test]
fn recursive_factorial() {
    assert_eq!(
        Value::Number(3628800.0),
        run_code_string(
            "(
                (fn factorial (n) (
                    (if (n == 1) (
                        n
                    ) else (
                        n * (factorial (n - 1))
                    ))
                ))

				(factorial 10)
            )",
        )
    );
}

#[test]
fn push_to_list() {
    assert_eq!(
        Value::List(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0)
        ]),
        run_code_string(
            "(
                (let list = [1 2])
				(list = (list + 3))
				list
            )",
        )
    );
}

#[allow(dead_code)]
const LIST_REMOVE_CODE: &str = "
(fn list_remove (list index_to_remove) (
		(let new_list = [])
		(for i in list (
			(if (i == index_to_remove) (

			) else (
				(new_list = (new_list + (get list i)))
			))
		))
		new_list
	))
";

#[test]
fn remove_index_from_list() {
    assert_eq!(
        Value::List(vec![Value::Number(1.0), Value::Number(3.0)]),
        run_code_string(
            format!(
                "(
					{LIST_REMOVE_CODE}
	
					(let list = [1 2 3])
					(list = (list_remove list 1))
					list
				)"
            )
            .as_str(),
        )
    );
}

#[allow(dead_code)]
const LIST_LEN_CODE: &str = "(fn list_len (list) (
	(let len = 0)
	(for i in list (
		(len = (len + 1))
	))
	len
))";

#[test]
fn get_list_length() {
    assert_eq!(
        Value::Number(3.0),
        run_code_string(
            format!(
                "(
					{LIST_LEN_CODE}
	
					(let list = [1 2 3])
					(list_len list)
				)"
            )
            .as_str(),
        )
    );
}

#[test]
fn pop_from_list() {
    assert_eq!(
        Value::List(vec![Value::Number(1.0), Value::Number(2.0)]),
        run_code_string(
            format!(
                "(
					{LIST_REMOVE_CODE}
					{LIST_LEN_CODE}

					(fn list_pop (list) (
						(list_remove list ((list_len list) - 1))
					))
	
					(let list = [1 2 3])
					(list = (list_pop list))
					list
				)"
            )
            .as_str(),
        )
    );
}

#[test]
fn iterate_string() {
    assert_eq!(
        Value::List(vec![Value::Char('a'), Value::Char('b'), Value::Char('c')]),
        run_code_string(
            format!(
                "(
                    (let string = \"abc\")
                    (let list = []);
					(for i in string (
                        (let char = (get string i))
                        (list = (list + char))
                    ))
                    list
				)"
            )
            .as_str(),
        )
    );
}

#[test]
fn concatenate_strings() {
    assert_eq!(
        Value::String(("banana").to_string()),
        run_code_string(
            format!(
                "(
                    ((\"ban\" + \"ana\")
				)"
            )
            .as_str(),
        )
    );
}

#[test]
fn concatenate_string_with_character() {
    assert_eq!(
        Value::String("banana".to_string()),
        run_code_string(
            format!(
                "(
                    ((\"ba\" + 'n') + ('a' + \"na\"))
				)"
            )
            .as_str(),
        )
    );
}
