use log;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use walkdir::WalkDir;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

// From: https://stackoverflow.com/a/47648303/98555
fn dedup<T: Eq + Hash + Clone>(v: &mut Vec<T>) {
    // note the Copy constraint
    let mut uniques = HashSet::new();
    v.retain(|e| uniques.insert(e.clone()));
}

// Scanning

type Iter<'a> = std::iter::Peekable<GraphemeIndices<'a>>;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Text(String),
    OpenLine,
    CloseLine,
    OpenValue,
    CloseValue,
    Identifier(String),
    Import,
    ImportDetails(String),
    With,
    As,
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
    UnexpectedGrapheme(String),
    UnexpectedEnd,
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

                    eat_spaces(iter);
                    let identifier = scan_identifier(iter);
                    tokens.push(Token::Identifier(identifier));
                    eat_spaces(iter);
                } else if let Some((_, "%")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push(Token::Text(buffer));
                        buffer = String::new();
                    }

                    tokens.push(Token::OpenStmt);
                    iter.next();

                    eat_spaces(iter);
                    let identifier = scan_identifier(iter);
                    let keyword_token = identifier_to_token(&identifier)?;
                    tokens.push(keyword_token.clone());

                    eat_spaces(iter);

                    match keyword_token {
                        Token::If => {
                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));
                        }
                        Token::For => {
                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));

                            eat_spaces(iter);

                            let identifier = scan_identifier(iter);
                            let keyword_token = identifier_to_token(&identifier)?;
                            tokens.push(keyword_token.clone());

                            eat_spaces(iter);

                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));
                        }
                        _ => {}
                    }

                    eat_spaces(iter);
                } else if let Some((_, ">")) = iter.peek() {
                    tokens.push(Token::OpenLine);
                    iter.next();

                    eat_spaces(iter);

                    let identifier = scan_identifier(iter);
                    let keyword_token = identifier_to_token(&identifier)?;
                    tokens.push(keyword_token.clone());

                    eat_spaces(iter);

                    match keyword_token {
                        Token::Import => {
                            let import_details = scan_import_details(iter);
                            tokens.push(Token::ImportDetails(import_details));
                        }
                        Token::With => {
                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));

                            eat_spaces(iter);

                            let identifier = scan_identifier(iter);
                            let keyword_token = identifier_to_token(&identifier)?;
                            tokens.push(keyword_token.clone());

                            eat_spaces(iter);

                            let identifier = scan_identifier(iter);
                            tokens.push(Token::Identifier(identifier));
                        }
                        _ => {}
                    }

                    consume_grapheme(iter, "\n")?;
                    tokens.push(Token::CloseLine);
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
    log::trace!("identifier_to_token: {}", identifier);
    match identifier {
        "if" => Ok(Token::If),
        "else" => Ok(Token::Else),
        "endif" => Ok(Token::EndIf),
        "for" => Ok(Token::For),
        "endfor" => Ok(Token::EndFor),
        "in" => Ok(Token::In),
        "import" => Ok(Token::Import),
        "with" => Ok(Token::With),
        "as" => Ok(Token::As),
        _ => Err(ScanError::UnknownKeyword(identifier.to_string())),
    }
}

fn scan_identifier(iter: &mut Iter) -> String {
    log::trace!("scan_identifier");
    let mut name = String::new();

    loop {
        match iter.peek() {
            Some((_index, "}")) | Some((_index, " ")) | Some((_index, "\n"))
            | Some((_index, "%")) => {
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

    name.trim().to_string()
}

fn scan_import_details(iter: &mut Iter) -> String {
    let mut details = String::new();

    loop {
        match iter.peek() {
            Some((_index, "\n")) => {
                break;
            }
            Some((_index, grapheme)) => {
                details.push_str(grapheme);
                iter.next();
            }
            None => {
                break;
            }
        }
    }

    details
}

fn consume_grapheme(iter: &mut Iter, expected: &str) -> Result<(), ScanError> {
    log::trace!("consume_grapheme");
    match iter.next() {
        Some((_, grapheme)) if grapheme == expected => Ok(()),
        entry => Err(entry
            .map(|(_, value)| ScanError::UnexpectedGrapheme(value.to_string()))
            .unwrap_or(ScanError::UnexpectedEnd)),
    }
}

fn eat_spaces(iter: &mut Iter) {
    log::trace!("eat_spaces");
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
    Identifier(String),
    If(String, Vec<Node>, Vec<Node>),
    For(String, String, Vec<Node>),
    Import(String),
    With(String, String),
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
                ast.push(Node::Identifier(name.clone()));
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
            Some(Token::OpenLine) => {
                match tokens.next() {
                    Some(Token::Import) => {
                        let import_details = extract_import_details(tokens)?;
                        ast.push(Node::Import(import_details))
                    }
                    Some(Token::With) => {
                        let identifier = extract_identifier(tokens)?;
                        consume_token(tokens, Token::As)?;
                        let type_ = extract_identifier(tokens)?;
                        ast.push(Node::With(identifier, type_))
                    }
                    _ => {}
                }
                consume_token(tokens, Token::CloseLine)?;
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

fn extract_import_details(tokens: &mut TokenIter) -> Result<String, ParserError> {
    log::trace!("extract_import_details");
    match tokens.next() {
        Some(Token::ImportDetails(details)) => Ok(details.clone()),
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
    let (builder_lines, params, imports, type_lookup) = render_lines(iter);

    let import_lines = imports
        .iter()
        .map(|details| format!("import {}", details))
        .collect::<Vec<_>>()
        .join("\n");

    let mut params = params
        .iter()
        .flat_map(|value| value.split(".").next())
        .collect::<Vec<_>>();

    dedup(&mut params);

    let params_string = params
        .iter()
        .map(|value| match type_lookup.get(*value) {
            Some(type_) => {
                format!("{} {}: {}", value, value, type_)
            }
            None => {
                format!("{} {}", value, value)
            }
        })
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

{}

pub fn render_builder({}) -> StringBuilder {{
    let builder = string_builder.from_string("")
{}
    builder
}}

pub fn render({}) -> String {{
    string_builder.to_string(render_builder({}))
}}
"#,
        import_lines, params_string, builder_lines, params_string, args_string
    );

    output
}

fn render_lines(
    iter: &mut NodeIter,
) -> (String, Vec<String>, Vec<String>, HashMap<String, String>) {
    let mut builder_lines = String::new();
    let mut params = vec![];
    let mut imports = vec![];
    let mut type_lookup = HashMap::new();

    loop {
        match iter.peek() {
            Some(Node::Text(text)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append(builder, \"{}\")\n",
                    text
                ));
            }
            Some(Node::Identifier(name)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append(builder, {})\n",
                    name
                ));
                params.push(name.clone());
            }
            Some(Node::Import(import_details)) => {
                iter.next();
                imports.push(import_details.clone());
            }
            Some(Node::With(identifier, type_)) => {
                iter.next();
                type_lookup.insert(identifier.clone(), type_.clone());
            }
            Some(Node::If(identifier_name, if_nodes, else_nodes)) => {
                iter.next();
                let (if_lines, mut if_params, _, _) = render_lines(&mut if_nodes.iter().peekable());
                let (else_lines, mut else_params, _, _) =
                    render_lines(&mut else_nodes.iter().peekable());
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
                let (loop_lines, mut _loop_params, _, _) =
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

    (builder_lines, params, imports, type_lookup)
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

    #[test]
    fn test_scan_dot_access() {
        assert_scan!("Hello{% if user.is_admin %} Admin{% endif %}");
    }

    #[test]
    fn test_scan_import() {
        assert_scan!("{> import user.{User}\n{{ name }}");
    }

    #[test]
    fn test_scan_with() {
        assert_scan!("{> with user as User\n{{ user }}");
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

    #[test]
    fn test_parse_dot_access() {
        assert_parse!("Hello{% if user.is_admin %} Admin{% endif %}");
    }

    #[test]
    fn test_parse_import() {
        assert_parse!("{> import user.{User}\n{{ name }}");
    }

    #[test]
    fn test_parse_with() {
        assert_parse!("{> with user as User\n{{ user }}");
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
    fn test_repeated_identifier_usage() {
        assert_render!("{{ name }} usage, {{ name }} usage");
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

    #[test]
    fn test_render_dot_access() {
        assert_render!("Hello{% if user.is_admin %} Admin{% endif %}");
    }

    #[test]
    fn test_render_import() {
        assert_render!("{> import user.{User}\n{{ name }}");
    }

    #[test]
    fn test_render_with() {
        assert_render!("{> with user as User\n{{ user }}");
    }

    #[test]
    fn test_render_import_and_with() {
        assert_render!("{> import user.{User}\n{> with user as User\n{{ user }}");
    }
}
