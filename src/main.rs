use log;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use walkdir::WalkDir;

// Scanning

type Iter<'a> = std::iter::Peekable<GraphemeIndices<'a>>;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Text(String),
    OpenValue,
    CloseValue,
    Identifier(String),
    OpenStmt,
    CloseStmt,
    If,
    Else,
    EndIf,
    For,
    EndFor,
    In,
}

#[derive(Debug)]
enum ScanError {
    UnknownKeyword(String),
}

fn scan(contents: &str) -> Result<Vec<Token>, ScanError> {
    log::trace!("scan");
    let iter = contents.grapheme_indices(true);
    let mut tokens = vec![];

    scan_plain(&mut iter.peekable(), &mut tokens)?;

    Ok(tokens)
}

fn scan_plain(iter: &mut Iter, tokens: &mut Vec<Token>) -> Result<(), ScanError> {
    log::trace!("scan_plain");
    let mut buffer = String::new();
    loop {
        match iter.peek() {
            Some((_index, "{")) => {
                iter.next();

                if let Some((_, "{")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push(Token::Text(buffer));
                        buffer = String::new();
                    }

                    tokens.push(Token::OpenValue);
                    iter.next();

                    eat_white_space(iter);
                    let identifier = scan_identifier(iter);
                    tokens.push(Token::Identifier(identifier));
                    eat_white_space(iter);
                } else if let Some((_, "%")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push(Token::Text(buffer));
                        buffer = String::new();
                    }

                    tokens.push(Token::OpenStmt);
                    iter.next();

                    eat_white_space(iter);
                    let identifier = scan_identifier(iter);
                    let keyword_token = identifier_to_token(&identifier)?;
                    tokens.push(keyword_token.clone());

                    match keyword_token {
                        Token::If => {
                            eat_white_space(iter);

                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));
                        }
                        Token::For => {
                            eat_white_space(iter);

                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));

                            eat_white_space(iter);

                            let identifier = scan_identifier(iter);
                            let keyword_token = identifier_to_token(&identifier)?;
                            tokens.push(keyword_token.clone());

                            eat_white_space(iter);

                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));
                        }
                        _ => {}
                    }

                    eat_white_space(iter);
                } else {
                    buffer.push('{');
                }
            }
            Some((_index, "}")) => {
                iter.next();

                if let Some((_, "}")) = iter.peek() {
                    tokens.push(Token::CloseValue);
                    iter.next();
                } else {
                    buffer.push('}');
                }
            }
            Some((_index, "%")) => {
                iter.next();

                if let Some((_, "}")) = iter.peek() {
                    tokens.push(Token::CloseStmt);
                    iter.next();
                } else {
                    buffer.push('%');
                }
            }
            Some((_index, grapheme)) => {
                buffer.push_str(grapheme);
                iter.next();
            }
            _ => {
                if !buffer.is_empty() {
                    tokens.push(Token::Text(buffer));
                }
                return Ok(());
            }
        }
    }
}

fn identifier_to_token(identifier: &str) -> Result<Token, ScanError> {
    match identifier {
        "if" => Ok(Token::If),
        "else" => Ok(Token::Else),
        "endif" => Ok(Token::EndIf),
        "for" => Ok(Token::For),
        "endfor" => Ok(Token::EndFor),
        "in" => Ok(Token::In),
        _ => Err(ScanError::UnknownKeyword(identifier.to_string())),
    }
}

fn scan_identifier(iter: &mut Iter) -> String {
    let mut name = String::new();

    loop {
        match iter.peek() {
            Some((_index, "}")) | Some((_index, " ")) | Some((_index, "%")) => {
                break;
            }
            Some((_index, grapheme)) => {
                name.push_str(grapheme);
                iter.next();
            }
            None => {
                break;
            }
        }
    }

    name
}

fn eat_white_space(iter: &mut Iter) {
    loop {
        match iter.peek() {
            Some((_index, " ")) => {
                iter.next();
            }
            _ => break,
        }
    }
}

// Parsing

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

#[derive(Debug)]
enum Node {
    Text(String),
    Indentifier(String),
    If(String, Vec<Node>, Vec<Node>),
    For(String, String, Vec<Node>),
}

#[derive(Debug)]
enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEnd,
}

fn parse(tokens: &mut TokenIter) -> Result<Vec<Node>, ParserError> {
    log::trace!("parse");
    parse_inner(tokens, false)
}

fn parse_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    match tokens.next() {
        Some(Token::If) => parse_if_statement(tokens),
        Some(Token::For) => parse_for_statement(tokens),
        Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
        None => {
            return Err(ParserError::UnexpectedEnd);
        }
    }
}

fn parse_inner(tokens: &mut TokenIter, in_statement: bool) -> Result<Vec<Node>, ParserError> {
    log::trace!("parse_inner");
    let mut ast = vec![];

    loop {
        match tokens.next() {
            Some(Token::Text(text)) => {
                ast.push(Node::Text(text.clone()));
            }
            Some(Token::OpenValue) => {
                let name = extract_identifier(tokens)?;
                ast.push(Node::Indentifier(name.clone()));
                consume_token(tokens, Token::CloseValue)?;
            }
            Some(Token::OpenStmt) => {
                if let Some(Token::Else) | Some(Token::EndIf) | Some(Token::EndFor) = tokens.peek()
                {
                    if in_statement {
                        break;
                    } else {
                        match tokens.next() {
                            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
                            None => return Err(ParserError::UnexpectedEnd),
                        }
                    }
                }
                let node = parse_statement(tokens)?;
                ast.push(node);
            }
            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
            None => {
                break;
            }
        }
    }

    Ok(ast)
}

fn parse_if_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    log::trace!("parse_if_statement");
    let name = extract_identifier(tokens)?;
    consume_token(tokens, Token::CloseStmt)?;

    let if_nodes = parse_inner(tokens, true)?;
    let mut else_nodes = vec![];

    match tokens.next() {
        Some(Token::EndIf) => {
            consume_token(tokens, Token::CloseStmt)?;
        }
        Some(Token::Else) => {
            consume_token(tokens, Token::CloseStmt)?;

            else_nodes = parse_inner(tokens, true)?;
            consume_token(tokens, Token::EndIf)?;
            consume_token(tokens, Token::CloseStmt)?;
        }
        Some(token) => {
            return Err(ParserError::UnexpectedToken(token.clone()));
        }
        None => {
            return Err(ParserError::UnexpectedEnd);
        }
    }

    Ok(Node::If(name, if_nodes, else_nodes))
}

fn parse_for_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    let entry_identifier = extract_identifier(tokens)?;
    consume_token(tokens, Token::In)?;

    let list_identifier = extract_identifier(tokens)?;
    consume_token(tokens, Token::CloseStmt)?;

    let loop_nodes = parse_inner(tokens, true)?;

    consume_token(tokens, Token::EndFor)?;
    consume_token(tokens, Token::CloseStmt)?;

    Ok(Node::For(entry_identifier, list_identifier, loop_nodes))
}

fn extract_identifier(tokens: &mut TokenIter) -> Result<String, ParserError> {
    log::trace!("extract_identifier");
    match tokens.next() {
        Some(Token::Identifier(name)) => Ok(name.clone()),
        Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
        None => Err(ParserError::UnexpectedEnd),
    }
}

fn consume_token(tokens: &mut TokenIter, token: Token) -> Result<(), ParserError> {
    log::trace!("consume_token");
    let next_token = tokens.next();
    if next_token == Some(&token) {
        Ok(())
    } else {
        Err(next_token
            .map(|token_value| ParserError::UnexpectedToken(token_value.clone()))
            .unwrap_or(ParserError::UnexpectedEnd))
    }
}

// Rendering

type NodeIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Node>>;

fn render(iter: &mut NodeIter) -> String {
    let (builder_lines, params) = render_lines(iter);

    let params_string = params
        .iter()
        .map(|value| format!("{} {}", value, value))
        .collect::<Vec<_>>()
        .join(", ");

    let args_string = params
        .iter()
        .map(|value| format!("{}: {}", value, value))
        .collect::<Vec<_>>()
        .join(", ");

    let output = format!(
        r#"import gleam/string_builder.{{StringBuilder}}
import gleam/list

pub fn render_builder({}) -> StringBuilder {{
    let builder = string_builder.from_string("")
{}
    builder
}}

pub fn render({}) -> String {{
    string_builder.to_string(render_builder({}))
}}
"#,
        params_string, builder_lines, params_string, args_string
    );

    output
}

fn render_lines(iter: &mut NodeIter) -> (String, Vec<String>) {
    let mut builder_lines = String::new();
    let mut params = vec![];

    loop {
        match iter.peek() {
            Some(Node::Text(text)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append(builder, \"{}\")\n",
                    text
                ));
            }
            Some(Node::Indentifier(name)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append(builder, {})\n",
                    name
                ));
                params.push(name.clone());
            }
            Some(Node::If(identifier_name, if_nodes, else_nodes)) => {
                iter.next();
                let (if_lines, mut if_params) = render_lines(&mut if_nodes.iter().peekable());
                let (else_lines, mut else_params) = render_lines(&mut else_nodes.iter().peekable());
                builder_lines.push_str(&format!(
                    r#"    let builder = case {} {{
        True -> {{
            {}
            builder
        }}
        False -> {{
            {}
            builder
        }}
}}
"#,
                    identifier_name, if_lines, else_lines
                ));
                params.push(identifier_name.clone());
                params.append(&mut if_params);
                params.append(&mut else_params);
            }
            Some(Node::For(entry_identifier, list_identifier, loop_nodes)) => {
                iter.next();
                let (loop_lines, mut _loop_params) =
                    render_lines(&mut loop_nodes.iter().peekable());
                builder_lines.push_str(&format!(
                    r#"    let builder = list.fold({}, builder, fn(builder, {}) {{
        {}
        builder
}})
"#,
                    list_identifier, entry_identifier, loop_lines
                ));
                params.push(list_identifier.clone());
            }
            None => break,
        }
    }

    (builder_lines, params)
}

fn convert(filepath: &std::path::PathBuf) {
    let contents = std::fs::read_to_string(filepath).expect("Unable to read file");
    let tokens = scan(&contents).expect("Unable to scan");
    let ast = parse(&mut tokens.iter().peekable()).expect("Unable to parse file");
    let output = render(&mut ast.iter().peekable());
    let mut out_file_path = filepath.clone();
    out_file_path.set_extension("gleam");
    std::fs::write(out_file_path, output).expect("Unable to write file");
}

fn main() {
    for entry in WalkDir::new(".").into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        if path.extension() == Some(std::ffi::OsStr::new("gleamx")) {
            println!("{}", path.display());
            convert(&path.to_path_buf());
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[macro_export]
    macro_rules! assert_scan {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format!("{:#?}", scan($text).expect("Scan failed")),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_parse {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let tokens = scan($text).expect("Scan failed");
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format!(
                    "{:#?}",
                    parse(&mut tokens.iter().peekable()).expect("Parse failed")
                ),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_render {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let tokens = scan($text).expect("Scan failed");
            let ast = parse(&mut tokens.iter().peekable()).expect("Parse failed");
            insta::assert_snapshot!(
                insta::internals::AutoName,
                render(&mut ast.iter().peekable()),
                $text
            );
        }};
    }

    // Scan

    #[test]
    fn test_scan_pure_text() {
        assert_scan!("Hello name, good to meet you");
    }

    #[test]
    fn test_scan_identifier() {
        assert_scan!("Hello {{ name }}, good to meet you");
    }

    #[test]
    fn test_scan_two_identifiers() {
        assert_scan!("Hello {{ name }}, {{ adjective }} to meet you");
    }

    #[test]
    fn test_scan_single_parens() {
        assert_scan!("Hello { name }, good to meet you");
    }

    #[test]
    fn test_scan_if_statement() {
        assert_scan!("Hello {% if is_user %}User{% endif %}");
    }

    #[test]
    fn test_scan_if_else_statement() {
        assert_scan!("Hello {% if is_user %}User{% else %}Unknown{% endif %}");
    }

    #[test]
    fn test_scan_nested_if_statements() {
        assert_scan!(
            "Hello {% if is_user %}{% if is_admin %}Admin{% else %}User{% endif %}{% endif %}"
        );
    }

    #[test]
    fn test_scan_for_loop() {
        assert_scan!("Hello {% for item in list %}{{ item }}{% endfor %}");
    }

    // Parse

    #[test]
    fn test_parse_pure_text() {
        assert_parse!("Hello name, good to meet you");
    }

    #[test]
    fn test_parse_single_parens() {
        assert_parse!("Hello { name }, good to meet you");
    }

    #[test]
    fn test_parse_identifier() {
        assert_parse!("Hello {{ name }}, good to meet you");
    }

    #[test]
    fn test_parse_if_statement() {
        assert_parse!("Hello {% if is_user %}User{% endif %}");
    }

    #[test]
    fn test_parse_empty_if_statement() {
        assert_parse!("Hello {% if is_user %}{% endif %}");
    }

    #[test]
    fn test_parse_if_else_statement() {
        assert_parse!("Hello {% if is_user %}User{% else %}Unknown{% endif %}");
    }

    #[test]
    fn test_parse_nested_if_statements() {
        assert_parse!(
            "Hello {% if is_user %}{% if is_admin %}Admin{% else %}User{% endif %}{% endif %}"
        );
    }

    #[test]
    fn test_parse_for_loop_with_single_identifier() {
        assert_parse!("Hello {% for item in list %}{{ item }}{% endfor %}");
    }

    #[test]
    fn test_parse_for_loop() {
        assert_parse!("Hello,{% for item in list %} to {{ item }} and {% endfor %} everyone else");
    }

    // Render

    #[test]
    fn test_render_pure_text() {
        assert_render!("Hello name, good to meet you");
    }

    #[test]
    fn test_render_identifier() {
        assert_render!("Hello {{ name }}, good to meet you");
    }

    #[test]
    fn test_render_two_identifiers() {
        assert_render!("Hello {{ name }}, {{ adjective }} to meet you");
    }

    #[test]
    fn test_render_if_statement() {
        assert_render!("Hello {% if is_user %}User{% endif %}");
    }

    #[test]
    fn test_render_empty_if_statement() {
        assert_render!("Hello {% if is_user %}{% endif %}");
    }

    #[test]
    fn test_render_if_else_statement() {
        assert_render!("Hello {% if is_user %}User{% else %}Unknown{% endif %}");
    }

    #[test]
    fn test_render_nested_if_statements() {
        assert_render!(
            "Hello {% if is_user %}{% if is_admin %}Admin{% else %}User{% endif %}{% endif %}"
        );
    }

    #[test]
    fn test_render_for_loop() {
        assert_render!("Hello,{% for item in list %} to {{ item }} and {% endfor %} everyone else");
    }
}
