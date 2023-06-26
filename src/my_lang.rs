use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

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
struct Token {
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

fn tokenize(text: &str) -> Vec<Token> {
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

fn parse_array(tokens: &[Token]) -> (TreeNode, usize) {
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

fn parse(tokens: &[Token]) -> TreeNode {
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
struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    variables: HashMap<String, Value>,
}

impl Scope {
    fn new(
        parent: Option<Rc<RefCell<Scope>>>,
        variables: HashMap<String, Value>,
    ) -> Scope {
        Scope { parent, variables }
    }

    fn get_variable_and_scope(&self, key: String) -> Option<(Value, Scope)> {
        match self.variables.get(&key) {
            Some(value) => Some((value.clone(), self.clone())),
            None => match &self.parent {
                None => None,
                Some(parent) => (*parent).borrow().get_variable_and_scope(key),
            },
        }
    }

    fn get_variable(&self, key: String) -> Option<Value> {
        match self.variables.get(&key) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                None => None,
                Some(parent) => (**parent).borrow().get_variable(key),
            },
        }
    }

    fn declare_variable(&mut self, key: String, value: Value) {
        self.variables.insert(key, value);
    }

    fn get_mut_parent(&mut self) -> Option<std::cell::RefMut<'_, Scope>> {
        match (&self.parent).as_deref() {
            None => None,
            Some(parent) => Some(parent.borrow_mut()),
        }
    }

    fn declare_variable_on_parent(&mut self, key: String, value: Value) {
        self.get_mut_parent().unwrap().declare_variable(key, value);
    }

    fn mutate_variable(&mut self, key: String, value: Value) {
        match self.variables.get(&key) {
            None => {
                self.get_mut_parent()
                    .expect("Variable not defined")
                    .mutate_variable(key, value);
            }
            Some(_) => self.declare_variable(key, value),
        };
    }
}

fn execute(tree_node: &TreeNode, parent_scope: Rc<RefCell<Scope>>) -> Value {
    let scope =
        Rc::new(RefCell::new(Scope::new(Some(parent_scope), HashMap::new())));

    match &tree_node.node_type {
        TreeNodeType::Paren(children) => match &children.as_slice() {
            [operand_1, TreeNode {
                node_type: TreeNodeType::Operator(operator),
            }, operand_2] => {
                if operator == "=" {
                    match &operand_1.node_type {
                        TreeNodeType::Literal(str) => {
                            let a = execute(operand_2, Rc::clone(&scope));
                            (*scope)
                                .borrow_mut()
                                .mutate_variable(str.to_string(), a);

                            return Value::Void;
                        }
                        _ => panic!(),
                    }
                }

                let operand_1 = execute(operand_1, Rc::clone(&scope));
                let operand_2 = execute(operand_2, Rc::clone(&scope));

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
            }] => execute(&children[0], scope),
            [TreeNode {
                node_type: TreeNodeType::Literal(literal),
            }, ..] => match literal.as_str() {
                "print" => {
                    let value = execute(&children[1], Rc::clone(&scope));
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

                        let value = execute(&arg2, Rc::clone(&scope));

                        (*scope).borrow_mut().declare_variable_on_parent(
                            arg1.to_string(),
                            value.clone(),
                        );

                        return value;
                    }
                    _ => panic!(),
                },
                "if" => match &children[1..] {
                    [condition, if_code, ..] => {
                        let condition_value =
                            execute(condition, Rc::clone(&scope));

                        match condition_value {
                            Value::Bool(true) => {
                                execute(if_code, Rc::clone(&scope))
                            }
                            Value::Bool(false) => match children.get(4) {
                                Some(else_code) => {
                                    execute(else_code, Rc::clone(&scope))
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

                        match execute(iterations, Rc::clone(&scope)) {
                            Value::Number(iterations) => {
                                let iterations = iterations.floor() as i32;
                                for i in 0..iterations {
                                    (*scope).borrow_mut().declare_variable(
                                        iteration_name.to_string(),
                                        Value::Number(i as f64),
                                    );
                                    execute(for_code, Rc::clone(&scope));
                                }

                                (*scope)
                                    .borrow_mut()
                                    .variables
                                    .remove(iteration_name);
                            }
                            Value::List(values) => {
                                let iterations = values.len();
                                for i in 0..iterations {
                                    (*scope).borrow_mut().declare_variable(
                                        iteration_name.to_string(),
                                        Value::Number(i as f64),
                                    );
                                    execute(for_code, Rc::clone(&scope));
                                }

                                (*scope)
                                    .borrow_mut()
                                    .variables
                                    .remove(iteration_name);
                            }
                            _ => panic!("This isn't iterable"),
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

                        (*scope).borrow_mut().declare_variable_on_parent(
                            name.to_string(),
                            function.clone(),
                        );

                        function
                    }
                    _ => panic!(),
                },
                "get" => match &children[1..] {
                    [arr, index] => match execute(arr, Rc::clone(&scope)) {
                        Value::List(values) => match execute(index, scope) {
                            Value::Number(number) => {
                                let a = number.floor() as usize;
                                values
                                    .get(a)
                                    .expect("List out of bounds")
                                    .clone()
                            }
                            _ => panic!(
                                "Get second parameter should be a number"
                            ),
                        },
                        _ => panic!("Get's first parameter should be a list"),
                    },
                    _ => panic!(),
                },
                _ => {
                    match (*scope)
                        .borrow()
                        .get_variable_and_scope(literal.to_string())
                        .expect(
                            format!("Function '{literal}' not defined")
                                .as_str(),
                        ) {
                        (Value::Function(args, code), mut func_scope) => {
                            for (i, key) in args.iter().enumerate() {
                                func_scope.declare_variable(
                                    key.to_string(),
                                    execute(
                                        &children[1..].get(i).expect(
                                            "Not enough arguments passed to function",
                                        ),
                                        Rc::clone(&scope),
                                    ),
                                )
                            }

                            func_scope.declare_variable(
                                literal.to_string(),
                                Value::Function(args, code.clone()),
                            );

                            execute(
                                &TreeNode { node_type: code },
                                Rc::new(RefCell::new(func_scope)),
                            )
                        }
                        _ => panic!("This is not a function"),
                    }
                }
            },
            [TreeNode {
                node_type: TreeNodeType::Paren(_),
            }] => execute(&children[0], Rc::clone(&scope)),
            [index_node, TreeNode {
                node_type: TreeNodeType::Literal(string),
            }] => match (*scope).borrow().get_variable(string.to_string()) {
                Some(value) => match value {
                    Value::List(values) => {
                        match execute(index_node, Rc::clone(&scope)) {
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
                let values = children
                    .iter()
                    .map(|child| execute(child, Rc::clone(&scope)));

                match values.last() {
                    Some(value) => value,
                    None => Value::Void,
                }
            }
        },
        TreeNodeType::Bool(bool) => Value::Bool(*bool),
        TreeNodeType::Number(number) => Value::Number(*number),
        TreeNodeType::Literal(variable) => {
            match (*scope).borrow().get_variable(variable.to_string()) {
                None => {
                    panic!(
                        "Variable '{variable}' not defined or is not in scope"
                    )
                }
                Some(value) => value.clone(),
            }
        }
        TreeNodeType::List(nodes) => Value::List(
            nodes
                .iter()
                .map(|node| execute(node, Rc::clone(&scope)))
                .collect::<Vec<_>>(),
        ),
        TreeNodeType::Operator(_) => panic!(),
    }
}

pub fn run_code_string(str: &str) -> Value {
    let tokens: Vec<Token> = tokenize(str);
    let ast = parse(&tokens.into_boxed_slice());
    execute(
        &ast,
        Rc::new(RefCell::new(Scope::new(None, HashMap::new()))),
    )
}
