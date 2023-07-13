use std::borrow::BorrowMut;

#[derive(Debug)]
enum TokenType {
	OpenParen,
	CloseParen,
	Operator(String),
	Literal(String),
	Number(f64),
}

#[derive(Debug)]
pub struct Token {
	token_type: TokenType,
}
impl Token {
	fn new(token_type: TokenType) -> Token {
		Token { token_type }
	}
}

enum IncompleteToken {
	None,
	Number(String),
	Literal(String),
}

pub fn tokenize(text: &str) -> Vec<Token> {
	let mut tokens = vec![];

	let mut state = IncompleteToken::None;

	for char in text.chars() {
		match char {
			'(' => tokens.push(Token::new(TokenType::OpenParen)),
			_ if char.is_numeric() => match state.borrow_mut() {
				IncompleteToken::None => {
					state = IncompleteToken::Number(char.to_string())
				}
				IncompleteToken::Number(string) => string.push(char),
				_ => panic!(),
			},
			_ if char.is_alphabetic() => match state {
				IncompleteToken::Literal(ref mut literal) => {
					literal.push(char);
				}
				_ => state = IncompleteToken::Literal(char.to_string()),
			},
			' ' | '\n' | ';' | ')' => {
				match state {
					IncompleteToken::None => {}
					IncompleteToken::Number(ref str) => {
						tokens.push(Token::new(TokenType::Number(
							str::parse(str.as_str()).unwrap(),
						)));
					}
					IncompleteToken::Literal(ref str) => {
						tokens.push(Token::new(TokenType::Literal(
							str.to_string(),
						)));
					}
				}

				state = IncompleteToken::None;

				if char == ')' {
					tokens.push(Token::new(TokenType::CloseParen))
				}
			}
			'+' | '-' | '*' | '/' => {
				tokens.push(Token::new(TokenType::Operator(char.to_string())))
			}
			_ => {
				panic!()
			}
		};
	}

	return tokens;
}

#[derive(Debug)]
enum TreeNodeType {
	Paren(Vec<TreeNode>),
	Operator(String),
	Number(f64),
	Literal(String),
}
#[derive(Debug)]
pub struct TreeNode {
	node_type: TreeNodeType,
}

pub fn parse(tokens: &[Token]) -> (TreeNode, usize) {
	let mut tree_node = TreeNode {
		node_type: TreeNodeType::Paren(vec![]),
	};

	let mut iter = tokens.iter().enumerate();

	while let Some((i, token)) = iter.next() {
		match &token.token_type {
			TokenType::OpenParen => match tree_node.node_type {
				TreeNodeType::Paren(ref mut children) => {
					let start_at = i + 1;

					let (parsed, end) = parse(&tokens[start_at..]);

					children.push(parsed);

					// skip loop to iteration "end"
					iter.nth(end);
				}
				_ => panic!(),
			},
			TokenType::CloseParen => return (tree_node, i),
			_ => match tree_node.node_type {
				TreeNodeType::Paren(ref mut children) => {
					match &token.token_type {
						TokenType::Operator(operator) => {
							children.push(TreeNode {
								node_type: TreeNodeType::Operator(
									operator.to_string(),
								),
							});
						}
						TokenType::Number(number) => {
							children.push(TreeNode {
								node_type: TreeNodeType::Number(*number),
							});
						}
						TokenType::Literal(literal) => {
							children.push(TreeNode {
								node_type: TreeNodeType::Literal(
									literal.to_string(),
								),
							});
						}
						_ => todo!(),
					}
				}
				_ => panic!(),
			},
		}
	}

	return (tree_node, tokens.len());
}

pub enum Value {
	Void,
	Number(f64),
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Void => write!(f, "()"),
			Value::Number(number) => {
				write!(f, "{number}")
			}
		}
	}
}

pub fn execute(tree_node: &TreeNode) -> Value {
	match &tree_node.node_type {
		TreeNodeType::Paren(children) => match &children.as_slice() {
			[TreeNode {
				node_type: TreeNodeType::Operator(operator),
			}, operand_1, operand_2] => {
				let operand_1 = match execute(operand_1) {
					Value::Number(n) => n,
					_ => panic!(),
				};

				let operand_2 = match execute(operand_2) {
					Value::Number(n) => n,
					_ => panic!(),
				};

				match operator.as_str() {
					"+" => Value::Number(operand_1 + operand_2),
					"-" => Value::Number(operand_1 - operand_2),
					"*" => Value::Number(operand_1 * operand_2),
					"/" => Value::Number(operand_1 / operand_2),
					_ => panic!(),
				}
			}
			[TreeNode {
				node_type: TreeNodeType::Literal(literal),
			}, ..] => match literal.as_str() {
				"print" => match children.get(1).unwrap().node_type {
					TreeNodeType::Paren(_) => {
						println!("{}", execute(&children[1]));
						Value::Void
					}
					TreeNodeType::Number(number) => {
						println!("{}", number);
						Value::Void
					}
					_ => panic!(),
				},
				_ => panic!(),
			},
			[TreeNode {
				node_type: TreeNodeType::Paren(_),
			}] => execute(&children[0]),
			_ => Value::Void,
		},
		TreeNodeType::Number(number) => Value::Number(*number),
		_ => {
			panic!()
		}
	}
}
