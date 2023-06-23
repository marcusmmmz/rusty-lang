use std::{borrow::BorrowMut, collections::HashMap};

#[derive(Debug)]
enum TokenType {
    OpenParen,
    CloseParen,
    OpenSquareBrackets,
    CloseSquareBrackets,
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

#[derive(Debug)]
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
            '[' => tokens.push(Token::new(TokenType::OpenSquareBrackets)),
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                match state.borrow_mut() {
                    IncompleteToken::None => {
                        state = IncompleteToken::Number(char.to_string())
                    }
                    IncompleteToken::Number(string) => string.push(char),
                    _ => panic!(),
                }
            }
            '_' => match state {
                IncompleteToken::Literal(ref mut literal) => {
                    literal.push(char);
                }
                _ => state = IncompleteToken::Literal(char.to_string()),
            },
            _ if char.is_alphabetic() => match state {
                IncompleteToken::Literal(ref mut literal) => {
                    literal.push(char);
                }
                _ => state = IncompleteToken::Literal(char.to_string()),
            },
            ' ' | '\n' | '\t' | ';' | ')' | ']' => {
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
                } else if char == ']' {
                    tokens.push(Token::new(TokenType::CloseSquareBrackets))
                }
            }
            '+' | '-' | '*' | '/' | '%' => {
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

#[derive(Clone, PartialEq, Debug)]
pub enum TreeNodeType {
    Paren(Vec<TreeNode>),
    Operator(String),
    Number(f64),
    Literal(String),
    Bool(bool),
    List(Vec<TreeNode>),
}
#[derive(PartialEq, Clone, Debug)]
pub struct TreeNode {
    node_type: TreeNodeType,
}

fn parse_simple(token: &Token) -> TreeNode {
    match &token.token_type {
        TokenType::Operator(operator) => TreeNode {
            node_type: TreeNodeType::Operator(operator.to_string()),
        },
        TokenType::Number(number) => TreeNode {
            node_type: TreeNodeType::Number(*number),
        },
        TokenType::Literal(literal) => TreeNode {
            node_type: TreeNodeType::Literal(literal.to_string()),
        },
        TokenType::Bool(bool) => TreeNode {
            node_type: TreeNodeType::Bool(*bool),
        },
        TokenType::OpenParen
        | TokenType::CloseParen
        | TokenType::OpenSquareBrackets
        | TokenType::CloseSquareBrackets => {
            panic!()
        }
    }
}

pub fn parse_array(tokens: &[Token]) -> (TreeNode, usize) {
    let mut tree_node = TreeNode {
        node_type: TreeNodeType::List(vec![]),
    };

    for (i, token) in tokens.iter().enumerate() {
        match token.token_type {
            TokenType::CloseSquareBrackets => return (tree_node, i),
            _ => match tree_node.node_type {
                TreeNodeType::List(ref mut values) => {
                    values.push(parse_simple(token))
                }
                _ => panic!(),
            },
        }
    }

    return (tree_node, tokens.len());
}

fn parse_paren(tokens: &[Token]) -> (TreeNode, usize) {
    let mut tree_node = TreeNode {
        node_type: TreeNodeType::Paren(vec![]),
    };

    let mut iter = tokens.iter().enumerate();

    while let Some((i, token)) = iter.next() {
        match &token.token_type {
            TokenType::OpenParen => match tree_node.node_type {
                TreeNodeType::Paren(ref mut children) => {
                    let start_at = i + 1;

                    let (parsed, end) = parse_paren(&tokens[start_at..]);

                    children.push(parsed);

                    // skip loop to iteration "end"
                    iter.nth(end);
                }
                _ => panic!(),
            },
            TokenType::CloseParen => return (tree_node, i),
            TokenType::OpenSquareBrackets => {
                match tree_node.node_type {
                    TreeNodeType::Paren(ref mut children) => {
                        let start_at = i + 1;

                        let (parsed, end) = parse_array(&tokens[start_at..]);

                        children.push(parsed);

                        // skip loop to iteration "end"
                        iter.nth(end);
                    }
                    _ => panic!(),
                }
            }
            _ => match tree_node.node_type {
                TreeNodeType::Paren(ref mut children) => {
                    children.push(parse_simple(token))
                }
                _ => panic!(),
            },
        }
    }

    return (tree_node, tokens.len());
}

pub fn parse(tokens: &[Token]) -> TreeNode {
    parse_paren(tokens).0
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Number(f64),
    Bool(bool),
    Function(Vec<String>, TreeNodeType),
    List(Vec<Value>),
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
            Value::Function(args, code) => {
                write!(f, "fn ({args:?}) {code:?}")
            }
            Value::List(list) => {
                let values = list
                    .iter()
                    .map(|value| (format!("{}", value)))
                    .collect::<Vec<_>>();

                write!(f, "[{}]", values.join(", "))
            }
        }
    }
}

#[derive(Clone)]
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

                match (operand_1, operand_2) {
                    (Value::Number(n1), Value::Number(n2)) => {
                        match operator.as_str() {
                            "+" => Value::Number(n1 + n2),
                            "-" => Value::Number(n1 - n2),
                            "*" => Value::Number(n1 * n2),
                            "/" => Value::Number(n1 / n2),
                            "%" => Value::Number(n1 % n2),
                            _ => panic!(),
                        }
                    }
                    (Value::List(values), new_value) => {
                        let mut cloned_values = values.clone();
                        cloned_values.push(new_value);
                        Value::List(cloned_values)
                    }
                    (new_value, Value::List(values)) => {
                        let mut cloned_values = values.clone();
                        cloned_values.push(new_value);
                        Value::List(cloned_values)
                    }
                    _ => panic!(),
                }
            }
            [TreeNode {
                node_type: TreeNodeType::Literal(_),
            }] => execute(&children[0], &mut scope),
            [TreeNode {
                node_type: TreeNodeType::Literal(literal),
            }, ..] => match literal.as_str() {
                "print" => {
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

                        match condition_value {
                            Value::Bool(true) => execute(if_code, &mut scope),
                            Value::Bool(false) => match children.get(4) {
                                Some(else_code) => {
                                    execute(else_code, &mut scope)
                                }
                                None => Value::Void,
                            },
                            _ => panic!("Condition is not a bool"),
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
                "fn" => match &children[1..] {
                    [TreeNode {
                        node_type: TreeNodeType::Literal(name),
                    }, TreeNode {
                        node_type: TreeNodeType::Paren(args),
                    }, TreeNode { node_type: fn_code }] => {
                        let args = args
                            .iter()
                            .map(|arg| match &arg.node_type {
                                TreeNodeType::Literal(str) => str.to_string(),
                                _ => panic!(),
                            })
                            .collect::<Vec<_>>();

                        let function = Value::Function(args, fn_code.clone());

                        parent_scope
                            .variables
                            .insert(name.to_string(), function.clone());

                        function
                    }
                    _ => panic!(),
                },
                _ => match scope.get_variable(literal.to_string()).unwrap() {
                    Value::Function(args, code) => {
                        // TODO: make this scope be defined in the function definition instead of the function call
                        let mut func_scope = scope.clone();

                        for (i, key) in args.iter().enumerate() {
                            let value = execute(
                                &children[1..].get(i).unwrap(),
                                &mut scope,
                            );

                            func_scope.variables.insert(key.to_string(), value);
                        }

                        func_scope.variables.insert(
                            literal.to_string(),
                            Value::Function(args, code.clone()),
                        );

                        execute(&TreeNode { node_type: code }, &mut func_scope)
                    }
                    _ => panic!(),
                },
            },
            [TreeNode {
                node_type: TreeNodeType::Paren(_),
            }] => execute(&children[0], &mut scope),
            [index_node, TreeNode {
                node_type: TreeNodeType::Literal(string),
            }] => match scope.get_variable(string.to_string()) {
                Some(value) => match value {
                    Value::List(values) => {
                        match execute(index_node, &mut scope) {
                            Value::Number(index) => values
                                .get(index.floor() as usize)
                                .unwrap()
                                .clone(),
                            _ => {
                                panic!("Lists can only be indexed with numbers")
                            }
                        }
                    }
                    _ => panic!(),
                },
                None => panic!(),
            },
            _ => {
                let values =
                    children.iter().map(|child| execute(child, &mut scope));

                match values.last() {
                    Some(value) => value,
                    None => Value::Void,
                }
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
        TreeNodeType::List(nodes) => Value::List(
            nodes
                .iter()
                .map(|node| execute(node, &mut scope))
                .collect::<Vec<_>>(),
        ),
        TreeNodeType::Operator(_) => panic!(),
    }
}
