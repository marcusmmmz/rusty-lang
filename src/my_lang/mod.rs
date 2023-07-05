mod tests;

#[derive(Debug)]
enum BinaryOperatorType {
    Equals,
    DoubleEquals,
}

#[derive(Debug)]
enum TokenType {
    Number(f32),
    Identifier(String),
    BinaryOperator(BinaryOperatorType),
    OpenParen,
    ClosedParen,
    OpenBracket,
    ClosedBracket,
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
}
impl Token {
    fn new(token_type: TokenType) -> Self {
        Token { token_type }
    }
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

    for char in text.chars() {
        match char {
            'a'..='z' | 'A'..='Z' => match state {
                State::None => state = State::Identifier(char.to_string()),
                State::Identifier(ref mut sequence) => sequence.push(char),
                _ => panic!(),
            },
            '0'..='9' => match state {
                State::None => state = State::Number(char.to_string()),
                State::Number(ref mut sequence) => sequence.push(char),
                State::Identifier(ref mut sequence) => sequence.push(char),
                _ => todo!(),
            },
            '=' => match state {
                State::None => {
                    state = State::BinaryOperator(BinaryOperatorType::Equals);
                }
                State::BinaryOperator(BinaryOperatorType::Equals) => {
                    state =
                        State::BinaryOperator(BinaryOperatorType::DoubleEquals);
                }
                _ => panic!("This is an invalid operator"),
            },
            ' ' | '\n' | '\r' | '\t' | ';' | '(' | ')' => {
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

#[derive(Debug)]
enum TreeNodeType {
    Brackets(Vec<TreeNode>),
    Number(f32),
    Identifier(String),
    LetDeclaration(String, Box<TreeNode>),
    IfStatement(Box<TreeNode>, Box<TreeNode>),
    WhileStatement(Box<TreeNode>, Box<TreeNode>),
    FunctionCall(String, Vec<TreeNode>),
}

#[derive(Debug)]
pub struct TreeNode {
    node_type: TreeNodeType,
}

impl TreeNode {
    fn new(node_type: TreeNodeType) -> Self {
        return TreeNode { node_type };
    }
}

fn parse_literal(token: &Token) -> Option<TreeNode> {
    match token.token_type {
        TokenType::Number(number) => {
            Some(TreeNode::new(TreeNodeType::Number(number)))
        }
        _ => None,
    }
}

fn parse_parenthesis(tokens: &[Token]) -> (Vec<TreeNode>, usize) {
    let mut children = vec![];

    for (i, token) in tokens.iter().enumerate() {
        match token.token_type {
            TokenType::ClosedParen => return (children, i),
            _ => {
                children.push(
                    parse_literal(token).expect("This is not allowed here"),
                );
            }
        }
    }

    panic!("Unfinished parenthesis");
}

fn parse_brackets(tokens: &[Token]) -> (TreeNode, usize) {
    enum State {
        None,
        LetDeclaration(Option<String>, bool),
        IfStatement(Option<TreeNode>, Option<TreeNode>),
        WhileStatement(Option<TreeNode>, Option<TreeNode>),
        Identifier(String), //Intermediary state, could be a function
    }

    let mut state = State::None;
    let mut children = vec![];

    let mut iter = tokens.iter().enumerate();

    while let Some((i, token)) = iter.next() {
        match &token.token_type {
            TokenType::Identifier(identifier) => match identifier.as_str() {
                "let" => match state {
                    State::None => state = State::LetDeclaration(None, false),
                    _ => panic!("'let' is invalid here"),
                },
                "if" => match state {
                    State::None => {
                        state = State::IfStatement(None, None);
                    }
                    _ => panic!(),
                },
                "while" => match state {
                    State::None => {
                        state = State::WhileStatement(None, None);
                    }
                    _ => panic!(),
                },
                _ => match state {
                    State::LetDeclaration(None, false) => {
                        state = State::LetDeclaration(
                            Some(identifier.clone()),
                            false,
                        )
                    }
                    State::LetDeclaration(Some(variable), true) => {
                        state = State::LetDeclaration(Some(variable), true)
                    }
                    State::IfStatement(None, None) => {
                        state = State::IfStatement(
                            Some(TreeNode::new(TreeNodeType::Identifier(
                                identifier.clone(),
                            ))),
                            None,
                        )
                    }
                    State::WhileStatement(None, None) => {
                        state = State::WhileStatement(
                            Some(TreeNode::new(TreeNodeType::Identifier(
                                identifier.clone(),
                            ))),
                            None,
                        )
                    }
                    State::None => {
                        state = State::Identifier(identifier.clone())
                    }
                    _ => panic!(),
                },
            },
            TokenType::BinaryOperator(binary_operator) => match binary_operator
            {
                BinaryOperatorType::Equals => match state {
                    State::LetDeclaration(Some(variable), false) => {
                        state = State::LetDeclaration(Some(variable), true)
                    }
                    _ => panic!("Missing variable name on variable declaraion"),
                },
                _ => todo!(),
            },
            TokenType::Number(number) => match state {
                State::LetDeclaration(Some(variable), true) => {
                    children.push(TreeNode::new(TreeNodeType::LetDeclaration(
                        variable,
                        Box::new(TreeNode::new(TreeNodeType::Number(
                            number.clone(),
                        ))),
                    )));
                    state = State::None;
                }
                _ => panic!("Missing = operator on variable declaraion"),
            },
            TokenType::OpenParen => match state {
                State::Identifier(func_name) => {
                    let start_at = i + 1;

                    let (args, end) = parse_parenthesis(&tokens[start_at..]);

                    children.push(TreeNode::new(TreeNodeType::FunctionCall(
                        func_name, args,
                    )));
                    state = State::None;

                    iter.nth(end);
                }
                _ => panic!(),
            },
            TokenType::ClosedParen => panic!("Unmatched parenthesis"),
            TokenType::OpenBracket => {
                let start_at = i + 1;

                let (brackets_children, end) =
                    parse_brackets(&tokens[start_at..]);

                children.push(match state {
                    State::IfStatement(Some(condition), None) => {
                        TreeNode::new(TreeNodeType::IfStatement(
                            Box::new(condition),
                            Box::new(brackets_children),
                        ))
                    }
                    State::WhileStatement(Some(condition), None) => {
                        TreeNode::new(TreeNodeType::WhileStatement(
                            Box::new(condition),
                            Box::new(brackets_children),
                        ))
                    }
                    _ => panic!(),
                });

                state = State::None;

                iter.nth(end);
            }
            TokenType::ClosedBracket => {
                return (TreeNode::new(TreeNodeType::Brackets(children)), i);
            }
        }
    }

    if !matches!(state, State::None) {
        panic!("The last expression is incomplete")
    }

    return (
        TreeNode::new(TreeNodeType::Brackets(children)),
        tokens.len(),
    );
}

fn parse(tokens: &[Token]) -> TreeNode {
    return parse_brackets(tokens).0;
}

fn ast_to_js(tree_node: &TreeNode) -> String {
    let mut js = String::new();

    match &tree_node.node_type {
        TreeNodeType::Brackets(children) => {
            js.push('{');
            let children_str = children
                .iter()
                .map(|child| ast_to_js(child))
                .collect::<Vec<_>>()
                .join("\n");

            js.push_str(children_str.as_str());
            js.push('}');
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
        TreeNodeType::LetDeclaration(variable, value) => {
            js.push_str("let ");
            js.push_str(variable);
            js.push_str(" = ");
            js.push_str(&ast_to_js(value));
        }
        TreeNodeType::Number(number) => js.push_str(&number.to_string()),
        TreeNodeType::Identifier(identifier) => {
            js.push_str(identifier.as_str())
        }
    }

    return js;
}

pub fn code_string_to_js(text: &str) -> String {
    let tokens = tokenize(text);
    let ast = parse(&tokens);
    let js = ast_to_js(&ast);

    return String::from(&js[1..(js.len() - 1)]);
}
