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
                _ => todo!(), // State::BinaryOperator(_) => todo!(),
            },
            '=' => match state {
                State::None => {
                    state = State::BinaryOperator(BinaryOperatorType::Equals)
                }
                State::BinaryOperator(BinaryOperatorType::Equals) => {
                    state =
                        State::BinaryOperator(BinaryOperatorType::DoubleEquals)
                }
                _ => panic!("This is an invalid operator"),
            },
            ' ' | '\n' | '\r' | '\t' | ';' => {
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
                    // _ => todo!(),
                }
                state = State::None
            }
            _ => panic!("Invalid character!"),
        }
    }

    return tokens;
}

#[derive(Debug)]
enum TreeNodeType {
    Block(Vec<TreeNode>),
    Number(f32),
    LetDeclaration(String, Box<TreeNode>),
}

#[derive(Debug)]
struct TreeNode {
    node_type: TreeNodeType,
}

impl TreeNode {
    fn new(node_type: TreeNodeType) -> Self {
        return TreeNode { node_type };
    }
}

fn parse(tokens: Vec<Token>) -> TreeNode {
    enum State {
        None,
        LetDeclaration(Option<String>, bool),
    }

    let mut state = State::None;
    let mut children = vec![];

    for token in tokens {
        match &token.token_type {
            TokenType::Identifier(identifier) => match identifier.as_str() {
                "let" => match state {
                    State::None => state = State::LetDeclaration(None, false),
                    _ => panic!("'let' is invalid here"),
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
                    _ => todo!(),
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
        }
    }

    if !matches!(state, State::None) {
        panic!("The last expression is incomplete")
    }

    return TreeNode::new(TreeNodeType::Block(children));
}

pub fn run_code_string(text: &str) {
    let tokens = tokenize(text);
    let ast = parse(tokens);
    dbg!(ast);
}
