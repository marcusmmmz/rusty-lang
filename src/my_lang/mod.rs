use std::{iter::Peekable, slice::Iter, str::Chars};

mod tests;

#[derive(Clone, Debug)]
enum BinaryOperatorType {
	Assignment,
	Equals,
	Less,
	Greater,
	Add,
	Subtract,
	Multiply,
	Divide,
}

#[derive(Clone, Debug)]
enum TokenType {
	Number(f32),
	Identifier(String),
	String(String),
	BinaryOperator(BinaryOperatorType),
	OpenParen,
	ClosedParen,
	OpenSquareBracket,
	Comma,
	Dot,
	ClosedSquareBracket,
	OpenBracket,
	ClosedBracket,
}

#[derive(Clone, Debug)]
struct Token {
	token_type: TokenType,
}
impl Token {
	fn new(token_type: TokenType) -> Self {
		Token { token_type }
	}
}

fn tokenize_string(iter: &mut Chars) -> Token {
	let mut string = String::new();

	for char in iter {
		if char == '"' {
			return Token::new(TokenType::String(string));
		} else {
			string.push(char);
		}
	}

	panic!("Unfinished string")
}

fn tokenize(text: &str) -> Vec<Token> {
	let text = text.to_owned() + "\n";

	enum State {
		None,
		Number(String),
		Identifier(String),
		BinaryOperator(BinaryOperatorType),
	}

	let mut state = State::None;
	let mut tokens = vec![];

	let mut iter = text.chars();

	while let Some(char) = iter.next() {
		match char {
			'a'..='z' | 'A'..='Z' => match state {
				State::None => state = State::Identifier(char.to_string()),
				State::Identifier(ref mut sequence) => sequence.push(char),
				_ => panic!(),
			},
			'"' => {
				tokens.push(tokenize_string(&mut iter));
			}
			'0'..='9' => match state {
				State::None => state = State::Number(char.to_string()),
				State::Number(ref mut sequence) => sequence.push(char),
				State::Identifier(ref mut sequence) => sequence.push(char),
				_ => todo!(),
			},
			'=' => match state {
				State::None => {
					state =
						State::BinaryOperator(BinaryOperatorType::Assignment);
				}
				State::BinaryOperator(BinaryOperatorType::Assignment) => {
					state = State::BinaryOperator(BinaryOperatorType::Equals);
				}
				_ => panic!("This is an invalid operator"),
			},
			'>' => match state {
				State::None => {
					state = State::BinaryOperator(BinaryOperatorType::Greater);
				}
				_ => panic!(),
			},
			'<' => match state {
				State::None => {
					state = State::BinaryOperator(BinaryOperatorType::Less);
				}
				_ => panic!(),
			},
			'+' | '-' | '*' | '/' => match state {
				State::None => {
					state = State::BinaryOperator(match char {
						'+' => BinaryOperatorType::Add,
						'-' => BinaryOperatorType::Subtract,
						'*' => BinaryOperatorType::Multiply,
						'/' => BinaryOperatorType::Divide,
						_ => panic!(),
					})
				}
				_ => panic!(),
			},
			' ' | '\n' | '\r' | '\t' | ';' | '(' | ')' | '[' | ']' | ','
			| '.' => {
				match state {
					State::None => {}
					State::Number(number) => tokens.push(Token::new(
						TokenType::Number(number.parse().unwrap()),
					)),
					State::Identifier(identifier) => tokens
						.push(Token::new(TokenType::Identifier(identifier))),
					State::BinaryOperator(operator_type) => tokens.push(
						Token::new(TokenType::BinaryOperator(operator_type)),
					),
				}
				state = State::None;

				match char {
					'(' => tokens.push(Token::new(TokenType::OpenParen)),
					')' => tokens.push(Token::new(TokenType::ClosedParen)),
					'[' => {
						tokens.push(Token::new(TokenType::OpenSquareBracket))
					}
					']' => {
						tokens.push(Token::new(TokenType::ClosedSquareBracket))
					}
					',' => tokens.push(Token::new(TokenType::Comma)),
					'.' => tokens.push(Token::new(TokenType::Dot)),
					_ => {}
				};
			}
			'{' => tokens.push(Token::new(TokenType::OpenBracket)),
			'}' => tokens.push(Token::new(TokenType::ClosedBracket)),
			_ => panic!("Invalid character!"),
		}
	}

	return tokens;
}

#[derive(Clone, Debug)]
enum TreeNodeType {
	Brackets(Vec<TreeNode>),
	Number(f32),
	Identifier(String),
	String(String),
	ReturnStatement(Box<TreeNode>),
	Member(Vec<TreeNode>),
	ArrayCreation(Vec<TreeNode>),
	BinaryOperation(Box<TreeNode>, BinaryOperatorType, Box<TreeNode>),
	LetStatement(String, Box<TreeNode>),
	IfStatement(Box<TreeNode>, Box<TreeNode>),
	WhileStatement(Box<TreeNode>, Box<TreeNode>),
	FunctionCall(String, Vec<TreeNode>),
	FunctionDeclaration(String, Vec<TreeNode>, Box<TreeNode>),
}

#[derive(Clone, Debug)]
pub struct TreeNode {
	node_type: TreeNodeType,
}

impl TreeNode {
	fn new(node_type: TreeNodeType) -> Self {
		return TreeNode { node_type };
	}
}

fn parse_return_statement<'a>(
	iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	TreeNode::new(TreeNodeType::ReturnStatement(Box::new(parse_expression(
		iter,
	))))
}

fn parse_square_brackets<'a>(iter: &mut Peekable<Iter<'a, Token>>) -> TreeNode {
	let mut nodes = vec![];

	if matches!(
		iter.peek().unwrap().token_type,
		TokenType::ClosedSquareBracket
	) {
		iter.next();
		return TreeNode::new(TreeNodeType::ArrayCreation(nodes));
	}

	loop {
		nodes.push(parse_expression(iter));

		match iter.peek().unwrap().token_type {
			TokenType::Comma => {
				iter.next();
				continue;
			}
			TokenType::ClosedSquareBracket => {
				iter.next();
				return TreeNode::new(TreeNodeType::ArrayCreation(nodes));
			}
			_ => panic!(),
		}
	}
}

fn try_parse_expr_unit<'a>(
	iter: &mut Peekable<Iter<'a, Token>>,
) -> Option<TreeNode> {
	let token = iter.peek().unwrap();

	let res = match &token.token_type {
		TokenType::Number(number) => {
			iter.next();
			Some(TreeNode::new(TreeNodeType::Number(*number)))
		}
		TokenType::String(string) => {
			iter.next();
			Some(TreeNode::new(TreeNodeType::String(string.clone())))
		}
		TokenType::Identifier(identifier) => {
			iter.next();
			match &iter.peek().unwrap().token_type {
				TokenType::OpenParen => {
					Some(parse_function_call(identifier.clone(), iter))
				}
				_ => Some(TreeNode::new(TreeNodeType::Identifier(
					identifier.clone(),
				))),
			}
		}
		TokenType::OpenSquareBracket => {
			iter.next();
			Some(parse_square_brackets(iter))
		}
		_ => None,
	};

	match res {
		None => None,
		Some(res) => Some(match iter.peek().unwrap().token_type {
			TokenType::Dot => parse_member(res, iter),
			_ => res,
		}),
	}
}

fn get_operator_precedence(operator: &BinaryOperatorType) -> i32 {
	match operator {
		BinaryOperatorType::Equals
		| BinaryOperatorType::Greater
		| BinaryOperatorType::Less
		| BinaryOperatorType::Assignment => 0,
		BinaryOperatorType::Add | BinaryOperatorType::Subtract => 1,
		BinaryOperatorType::Multiply | BinaryOperatorType::Divide => 2,
	}
}

fn parse_known_size_expression(nodes: &[TreeNode]) -> TreeNode {
	if nodes.len() == 1 {
		return nodes.get(0).unwrap().clone();
	};

	let mut lowest_precedence = 9999;
	let mut lowest_precedence_index = 0;
	let mut lowest_precedence_operator = None;

	for (i, node) in nodes.iter().enumerate() {
		match &node.node_type {
			TreeNodeType::BinaryOperation(_, operator, _) => {
				let op_precedence = get_operator_precedence(&operator);

				if op_precedence < lowest_precedence {
					lowest_precedence = op_precedence;
					lowest_precedence_index = i;
					lowest_precedence_operator = Some(operator)
				}
			}
			_ => {}
		};
	}

	TreeNode::new(TreeNodeType::BinaryOperation(
		Box::new(parse_known_size_expression(
			&nodes[..lowest_precedence_index],
		)),
		lowest_precedence_operator.unwrap().clone(),
		Box::new(parse_known_size_expression(
			&nodes[lowest_precedence_index + 1..],
		)),
	))
}

fn parse_expression<'a>(iter: &mut Peekable<Iter<'a, Token>>) -> TreeNode {
	let first_unit = try_parse_expr_unit(iter).unwrap();

	let Some(token) = iter.peek() else {
		return first_unit;
	};

	match token.token_type {
		TokenType::BinaryOperator(_) => {
			let mut nodes = vec![first_unit];

			loop {
				match &iter.peek().unwrap().token_type {
					TokenType::BinaryOperator(operator) => {
						iter.next();
						nodes.push(TreeNode::new(
							TreeNodeType::BinaryOperation(
								Box::new(TreeNode::new(TreeNodeType::Number(
									0.0,
								))),
								operator.clone(),
								Box::new(TreeNode::new(TreeNodeType::Number(
									0.0,
								))),
							),
						))
					}
					_ => {
						match try_parse_expr_unit(iter) {
							None => break,
							Some(node) => {
								nodes.push(node);
							}
						};
					}
				}
			}

			parse_known_size_expression(&nodes)
		}
		_ => first_unit,
	}
}

fn parse_parenthesis<'a>(
	iter: &mut Peekable<Iter<'a, Token>>,
) -> Vec<TreeNode> {
	let mut children = vec![];

	match iter.next().unwrap().token_type {
		TokenType::OpenParen => {}
		_ => panic!(),
	};

	loop {
		let token = iter.peek().expect("Unfinished parenthesis");

		match token.token_type {
			TokenType::ClosedParen => {
				iter.next();

				return children;
			}
			_ => {
				children.push(parse_expression(iter));
			}
		};
	}
}

fn parse_let_statement<'a>(
	mut iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	let identifier = match &iter.next().unwrap().token_type {
		TokenType::Identifier(identifier) => identifier,
		_ => panic!(),
	};

	match iter.next().unwrap().token_type {
		TokenType::BinaryOperator(BinaryOperatorType::Assignment) => {}
		_ => panic!(),
	};

	let expression = parse_expression(&mut iter);

	return TreeNode::new(TreeNodeType::LetStatement(
		identifier.clone(),
		Box::new(expression),
	));
}

fn parse_if_statement<'a>(
	mut iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	let conditional = parse_expression(&mut iter);
	let brackets = parse_brackets(&mut iter);

	return TreeNode::new(TreeNodeType::IfStatement(
		Box::new(conditional),
		Box::new(brackets),
	));
}

fn parse_while_statement<'a>(
	mut iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	let conditional = parse_expression(&mut iter);
	let brackets = parse_brackets(&mut iter);

	return TreeNode::new(TreeNodeType::WhileStatement(
		Box::new(conditional),
		Box::new(brackets),
	));
}

fn parse_function_call<'a>(
	func_name: String,
	mut iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	let args = parse_parenthesis(&mut iter);

	return TreeNode::new(TreeNodeType::FunctionCall(func_name, args));
}

fn parse_member<'a>(
	object: TreeNode,
	iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	match iter.next().unwrap().token_type {
		TokenType::Dot => {}
		_ => panic!(),
	}

	let mut vec = vec![object];

	loop {
		let expr_unit = try_parse_expr_unit(iter).unwrap();

		vec.push(expr_unit);

		match iter.peek().unwrap().token_type {
			TokenType::Dot => iter.next(),
			_ => break,
		};
	}

	TreeNode::new(TreeNodeType::Member(vec))
}

fn parse_function_declaration<'a>(
	mut iter: &mut Peekable<Iter<'a, Token>>,
) -> TreeNode {
	let func_name = match &iter.next().unwrap().token_type {
		TokenType::Identifier(identifier) => identifier.clone(),
		_ => panic!(),
	};

	let args = parse_parenthesis(&mut iter);

	let code = parse_brackets(&mut iter);

	return TreeNode::new(TreeNodeType::FunctionDeclaration(
		func_name,
		args,
		Box::new(code),
	));
}

fn parse_brackets<'a>(mut iter: &mut Peekable<Iter<'a, Token>>) -> TreeNode {
	match iter.next().unwrap().token_type {
		TokenType::OpenBracket => {}
		_ => panic!(),
	}

	let mut children = vec![];

	while let Some(token) = iter.peek() {
		macro_rules! parse_and_skip {
			($parse_func: ident) => {{
				let tree_node = $parse_func(&mut iter);

				children.push(tree_node);
			}};
		}

		match &token.token_type {
			TokenType::Identifier(identifier) => match identifier.as_str() {
				"let" => {
					iter.next();
					parse_and_skip!(parse_let_statement)
				}
				"if" => {
					iter.next();
					parse_and_skip!(parse_if_statement)
				}
				"while" => {
					iter.next();
					parse_and_skip!(parse_while_statement)
				}
				"fn" => {
					iter.next();
					parse_and_skip!(parse_function_declaration)
				}
				"return" => {
					iter.next();
					parse_and_skip!(parse_return_statement)
				}
				_ => parse_and_skip!(parse_expression),
			},
			_ => match &iter.next().unwrap().token_type {
				TokenType::Identifier(_) => panic!(),
				TokenType::OpenSquareBracket => {
					parse_and_skip!(parse_expression)
				}
				TokenType::ClosedSquareBracket => {
					panic!("Unmatched square brackets")
				}
				TokenType::ClosedParen => panic!("Unmatched parenthesis"),
				TokenType::OpenBracket => parse_and_skip!(parse_brackets),
				TokenType::ClosedBracket => {
					return TreeNode::new(TreeNodeType::Brackets(children));
				}
				TokenType::String(string) => {
					return TreeNode::new(TreeNodeType::String(string.clone()));
				}
				TokenType::BinaryOperator(_)
				| TokenType::Comma
				| TokenType::Dot
				| TokenType::Number(_)
				| TokenType::OpenParen => panic!(),
			},
		}
	}

	panic!("Unmatched brackets")
}

fn parse(tokens: &[Token]) -> TreeNode {
	let tokens = [
		&[Token::new(TokenType::OpenBracket)],
		tokens,
		&[Token::new(TokenType::ClosedBracket)],
	]
	.concat();

	return parse_brackets(&mut tokens.iter().peekable());
}

fn ast_to_js(tree_node: &TreeNode) -> String {
	let mut js = String::new();

	match &tree_node.node_type {
		TreeNodeType::ReturnStatement(expr) => {
			js.push_str("return ");
			js.push_str(&ast_to_js(expr));
		}
		TreeNodeType::Brackets(children) => {
			js.push_str("{\n");
			let children_str = children
				.iter()
				.map(|child| ast_to_js(child))
				.collect::<Vec<_>>()
				.join(";\n");

			js.push_str(children_str.as_str());
			js.push_str("\n}");
		}
		TreeNodeType::Member(children) => {
			let children_str = children
				.iter()
				.map(|child| ast_to_js(child))
				.collect::<Vec<_>>()
				.join(".");

			js.push_str(children_str.as_str());
		}
		TreeNodeType::ArrayCreation(children) => {
			js.push_str("[");
			let children_str = children
				.iter()
				.map(|child| ast_to_js(child))
				.collect::<Vec<_>>()
				.join(", ");

			js.push_str(children_str.as_str());
			js.push_str("]");
		}
		TreeNodeType::FunctionCall(func_name, args) => {
			js.push_str(match func_name.as_str() {
				"print" => "console.log",
				_ => &func_name,
			});

			js.push('(');

			let args_str = args
				.iter()
				.map(|arg| ast_to_js(arg))
				.collect::<Vec<_>>()
				.join(",");

			js.push_str(args_str.as_str());

			js.push(')');
		}
		TreeNodeType::FunctionDeclaration(func_name, args, code) => {
			js.push_str("function ");

			js.push_str(&func_name);

			js.push('(');

			let args_str = args
				.iter()
				.map(|arg| ast_to_js(arg))
				.collect::<Vec<_>>()
				.join(",");

			js.push_str(args_str.as_str());

			js.push_str(") ");

			js.push_str(&ast_to_js(code));
		}
		TreeNodeType::IfStatement(condition, brackets) => {
			js.push_str("if (");
			js.push_str(&ast_to_js(&condition));
			js.push_str(") ");
			js.push_str(&ast_to_js(&brackets));
		}
		TreeNodeType::WhileStatement(condition, brackets) => {
			js.push_str("while (");
			js.push_str(&ast_to_js(&condition));
			js.push_str(") ");
			js.push_str(&ast_to_js(&brackets));
		}
		TreeNodeType::LetStatement(variable, value) => {
			js.push_str("let ");
			js.push_str(variable);
			js.push_str(" = ");
			js.push_str(&ast_to_js(value));
		}
		TreeNodeType::Number(number) => js.push_str(&number.to_string()),
		TreeNodeType::Identifier(identifier) => {
			js.push_str(identifier.as_str())
		}
		TreeNodeType::String(string) => {
			js.push('"');
			js.push_str(string.as_str());
			js.push('"');
		}
		TreeNodeType::BinaryOperation(expr1, operator, expr2) => {
			let operator_str = match operator {
				BinaryOperatorType::Assignment => "=",
				BinaryOperatorType::Equals => "==",
				BinaryOperatorType::Greater => ">",
				BinaryOperatorType::Less => "<",
				BinaryOperatorType::Add => "+",
				BinaryOperatorType::Subtract => "-",
				BinaryOperatorType::Multiply => "*",
				BinaryOperatorType::Divide => "/",
			};

			let expr1 = &ast_to_js(expr1);
			let expr2 = &ast_to_js(expr2);

			js.push_str(&format!("{expr1} {operator_str} {expr2}"));
		}
	}

	return js;
}

pub fn code_string_to_js(text: &str) -> String {
	let tokens = tokenize(text);
	let ast = parse(&tokens);
	let js = ast_to_js(&ast);

	return String::from(&js[2..(js.len() - 2)]);
}
