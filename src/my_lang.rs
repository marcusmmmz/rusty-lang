use std::{borrow::BorrowMut, collections::HashMap};

#[derive(Debug)]
enum TokenType {
    OpenParen,
    CloseParen,
    Operator(String),
    Literal(String),
    Number(f64),
    Bool(bool),
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
    Operator(String),
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

            ' ' | '\n' | '\t' | ';' | ')' => {
                match state {
                    IncompleteToken::None => {}
                    IncompleteToken::Number(ref str) => {
                        tokens.push(Token::new(TokenType::Number(
                            str::parse(str.as_str()).unwrap(),
                        )));
                    }
                    IncompleteToken::Literal(ref str) => {
                        tokens.push(Token::new(match str.as_str() {
                            "true" => TokenType::Bool(true),
                            "false" => TokenType::Bool(false),
                            _ => TokenType::Literal(str.to_string()),
                        }));
                    }
                    IncompleteToken::Operator(ref str) => {
                        tokens.push(Token::new(TokenType::Operator(
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
            '!' | '=' => match state {
                IncompleteToken::None => {
                    state = IncompleteToken::Operator(char.to_string())
                }
                IncompleteToken::Operator(ref operator) => {
                    tokens.push(Token::new(TokenType::Operator(
                        match operator.as_str() {
                            "=" => "==".to_string(),
                            "!" => "!=".to_string(),
                            _ => panic!(),
                        },
                    )));
                    state = IncompleteToken::None
                }
                _ => {}
            },
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
    Bool(bool),
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
                        TokenType::Bool(bool) => {
                            children.push(TreeNode {
                                node_type: TreeNodeType::Bool(*bool),
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

#[derive(Clone, PartialEq)]
pub enum Value {
    Void,
    Number(f64),
    Bool(bool),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "()"),
            Value::Number(number) => {
                write!(f, "{number}")
            }
            Value::Bool(bool) => {
                write!(f, "{bool}")
            }
        }
    }
}

pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    variables: HashMap<String, Value>,
}

impl Scope<'_> {
    pub fn new<'a>(
        parent: Option<&'a Scope<'a>>,
        variables: HashMap<String, Value>,
    ) -> Scope<'a> {
        Scope { parent, variables }
    }

    pub fn get_variable(&self, key: String) -> Option<Value> {
        match self.variables.get(&key) {
            Some(value) => Some(value.clone()),
            None => match self.parent {
                None => None,
                Some(parent) => match parent.get_variable(key) {
                    None => None,
                    Some(value) => Some(value),
                },
            },
        }
    }
}

pub fn execute(tree_node: &TreeNode, parent_scope: &mut Scope) -> Value {
    let mut scope = Scope::new(Some(parent_scope), HashMap::new());

    match &tree_node.node_type {
        TreeNodeType::Paren(children) => match &children.as_slice() {
            [operand_1, TreeNode {
                node_type: TreeNodeType::Operator(operator),
            }, operand_2] => {
                let operand_1 = execute(operand_1, &mut scope);
                let operand_2 = execute(operand_2, &mut scope);

                if operator == "==" {
                    return Value::Bool(operand_1 == operand_2);
                }

                let operand_1 = match operand_1 {
                    Value::Number(n) => n,
                    _ => panic!(),
                };

                let operand_2: f64 = match operand_2 {
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
                "print" => {
                    // dbg!("print");
                    let value = execute(&children[1], &mut scope);
                    println!("{value}");
                    value
                }
                "let" => match &children[1..] {
                    [TreeNode {
                        node_type: TreeNodeType::Literal(arg1),
                    }, TreeNode {
                        node_type: TreeNodeType::Operator(operator),
                    }, arg2] => {
                        // dbg!("let");
                        if operator != "=" {
                            panic!()
                        }

                        let value = execute(&arg2, &mut scope);

                        parent_scope
                            .variables
                            .insert(arg1.to_string(), value.clone());

                        return value;
                    }
                    _ => panic!(),
                },
                "if" => match &children[1..] {
                    [condition, if_code, ..] => {
                        let condition_value = execute(condition, &mut scope);

                        // println!("{condition_value}");

                        match condition_value {
                            Value::Bool(true) => execute(if_code, &mut scope),
                            Value::Bool(false) => match &children[4..] {
                                [else_code] => execute(else_code, &mut scope),
                                _ => Value::Void,
                            },
                            _ => Value::Void,
                        }
                    }
                    _ => panic!(),
                },
                "for" => match &children[1..] {
                    [TreeNode {
                        node_type: TreeNodeType::Literal(iteration_name),
                    }, TreeNode {
                        node_type: TreeNodeType::Literal(operator),
                    }, iterations, for_code] => {
                        if operator != "in" {
                            panic!()
                        }

                        match execute(iterations, &mut scope) {
                            Value::Number(iterations) => {
                                let iterations = iterations.floor() as i32;
                                for i in 0..iterations {
                                    scope.variables.insert(
                                        iteration_name.to_string(),
                                        Value::Number(i as f64),
                                    );
                                    execute(for_code, &mut scope);
                                }
                                scope.variables.remove(iteration_name);
                            }
                            _ => panic!(),
                        }

                        Value::Void
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            [TreeNode {
                node_type: TreeNodeType::Paren(_),
            }] => execute(&children[0], &mut scope),
            _ => {
                for child in children {
                    execute(child, &mut scope);
                }
                Value::Void
            }
        },
        TreeNodeType::Bool(bool) => Value::Bool(*bool),
        TreeNodeType::Number(number) => Value::Number(*number),
        TreeNodeType::Literal(variable) => {
            match scope.get_variable(variable.to_string()) {
                None => panic!(),
                Some(value) => value.clone(),
            }
        }
        _ => {
            panic!()
        }
    }
}