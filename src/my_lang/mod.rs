mod tests;

#[derive(Clone, Debug)]
enum BinaryOperatorType {
    Equals,
    DoubleEquals,
}

#[derive(Clone, Debug)]
enum TokenType {
    Number(f32),
    Identifier(String),
    BinaryOperator(BinaryOperatorType),
    OpenParen,
    ClosedParen,
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
    LetStatement(String, Box<TreeNode>),
    Assignment(String, Box<TreeNode>),
    IfStatement(Box<TreeNode>, Box<TreeNode>),
    WhileStatement(Box<TreeNode>, Box<TreeNode>),
    FunctionCall(String, Vec<TreeNode>),
    FunctionDeclaration(String, Vec<TreeNode>, Box<TreeNode>),
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

fn parse_expression(tokens: &[Token]) -> (TreeNode, usize) {
    match &tokens[0].token_type {
        TokenType::Number(number) => {
            ((TreeNode::new(TreeNodeType::Number(*number))), 1)
        }
        TokenType::Identifier(identifier) => (
            (TreeNode::new(TreeNodeType::Identifier(identifier.clone()))),
            1,
        ),
        _ => {
            panic!("This should be an expression")
        }
    }
}

fn parse_parenthesis(tokens: &[Token]) -> (Vec<TreeNode>, usize) {
    let mut children = vec![];

    for (i, token) in tokens.iter().enumerate() {
        match token.token_type {
            TokenType::ClosedParen => return (children, i),
            _ => {
                children.push(parse_expression(&[token.clone()]).0);
            }
        }
    }

    panic!("Unfinished parenthesis");
}

fn parse_let_statement(tokens: &[Token]) -> (TreeNode, usize) {
    let identifier = match &tokens[0].token_type {
        TokenType::Identifier(identifier) => identifier,
        _ => panic!(),
    };

    match tokens[1].token_type {
        TokenType::BinaryOperator(BinaryOperatorType::Equals) => {}
        _ => panic!(),
    };

    let (expression, expression_end) = parse_expression(&tokens[2..]);

    return (
        TreeNode::new(TreeNodeType::LetStatement(
            identifier.clone(),
            Box::new(expression),
        )),
        2 + expression_end,
    );
}

fn parse_assignment(tokens: &[Token]) -> (TreeNode, usize) {
    let identifier = match &tokens[0].token_type {
        TokenType::Identifier(identifier) => identifier,
        _ => panic!(),
    };

    match tokens[1].token_type {
        TokenType::BinaryOperator(BinaryOperatorType::Equals) => {}
        _ => panic!(),
    };

    let (expression, expression_end) = parse_expression(&tokens[2..]);

    return (
        TreeNode::new(TreeNodeType::Assignment(
            identifier.clone(),
            Box::new(expression),
        )),
        2 + expression_end,
    );
}

fn parse_if_statement(tokens: &[Token]) -> (TreeNode, usize) {
    let (conditional, conditional_end) = parse_expression(tokens);
    let (brackets, brackets_end) =
        parse_brackets(&tokens[conditional_end + 1..]);

    return (
        TreeNode::new(TreeNodeType::IfStatement(
            Box::new(conditional),
            Box::new(brackets),
        )),
        conditional_end + brackets_end,
    );
}

fn parse_while_statement(tokens: &[Token]) -> (TreeNode, usize) {
    let (conditional, conditional_end) = parse_expression(tokens);
    let (brackets, brackets_end) =
        parse_brackets(&tokens[conditional_end + 1..]);

    return (
        TreeNode::new(TreeNodeType::WhileStatement(
            Box::new(conditional),
            Box::new(brackets),
        )),
        conditional_end + brackets_end,
    );
}

fn parse_function_call(tokens: &[Token]) -> (TreeNode, usize) {
    let func_name = match &tokens[0].token_type {
        TokenType::Identifier(identifier) => identifier.clone(),
        _ => panic!(),
    };
    let (args, paren_end) = parse_parenthesis(&tokens[2..]);

    return (
        TreeNode::new(TreeNodeType::FunctionCall(func_name, args)),
        1 + paren_end,
    );
}

fn parse_function_declaration(tokens: &[Token]) -> (TreeNode, usize) {
    let func_name = match &tokens[0].token_type {
        TokenType::Identifier(identifier) => identifier.clone(),
        _ => panic!(),
    };

    let (args, args_end) = parse_parenthesis(&tokens[2..]);

    let (code, code_end) = parse_brackets(&tokens[4 + args_end..]);

    return (
        TreeNode::new(TreeNodeType::FunctionDeclaration(
            func_name,
            args,
            Box::new(code),
        )),
        1 + 2 + args_end + 2 + code_end,
    );
}

fn parse_unknown_identifier(tokens: &[Token]) -> (TreeNode, usize) {
    return match &tokens[1].token_type {
        TokenType::BinaryOperator(BinaryOperatorType::Equals) => {
            parse_assignment(tokens)
        }
        TokenType::OpenParen => parse_function_call(tokens),
        _ => panic!(),
    };
}

fn parse_brackets(tokens: &[Token]) -> (TreeNode, usize) {
    let mut children = vec![];
    let mut iter = tokens.iter().enumerate();

    while let Some((i, token)) = iter.next() {
        macro_rules! parse_and_skip {
            ($parse_func: ident, $tokens_to_skip: expr) => {{
                let (tree_node, end) =
                    $parse_func(&tokens[i + $tokens_to_skip..]);

                children.push(tree_node);

                iter.nth(end);
            }};
        }

        match &token.token_type {
            TokenType::Identifier(identifier) => match identifier.as_str() {
                "let" => parse_and_skip!(parse_let_statement, 1),
                "if" => parse_and_skip!(parse_if_statement, 1),
                "while" => parse_and_skip!(parse_while_statement, 1),
                "fn" => parse_and_skip!(parse_function_declaration, 1),
                _ => parse_and_skip!(parse_unknown_identifier, 0),
            },
            TokenType::ClosedParen => panic!("Unmatched parenthesis"),
            TokenType::OpenBracket => parse_and_skip!(parse_brackets, 1),
            TokenType::ClosedBracket => {
                return (TreeNode::new(TreeNodeType::Brackets(children)), i);
            }
            TokenType::BinaryOperator(_)
            | TokenType::Number(_)
            | TokenType::OpenParen => panic!(),
        }
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
        TreeNodeType::Assignment(variable, value) => {
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
