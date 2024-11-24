use crate::scanner::{Range, Token};

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, (Token, Range)>>;

type Type = String;

#[derive(Debug)]
pub enum Node {
    Text(String),
    Identifier(String),
    Builder(String),
    If(String, Vec<Node>, Vec<Node>),
    For(String, Option<Type>, String, Vec<Node>),
    Import(String),
    With((String, Range), Type),
    BlockFunction(Visibility, String, Vec<Node>, Range),
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, Range, Vec<Token>),
    UnexpectedEnd,
    FunctionWithinStatement(Range),
}

#[derive(Debug)]
pub enum Visibility {
    Public,
    Private,
}

pub fn parse(tokens: &mut TokenIter) -> Result<Vec<Node>, ParserError> {
    log::trace!("parse");
    parse_inner(tokens, false)
}

fn parse_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    log::trace!("parse_statement");
    match tokens.next() {
        Some((Token::If, _)) => parse_if_statement(tokens),
        Some((Token::For, _)) => parse_for_statement(tokens),
        Some((token, range)) => Err(ParserError::UnexpectedToken(
            token.clone(),
            range.clone(),
            vec![Token::If, Token::For],
        )),
        None => Err(ParserError::UnexpectedEnd),
    }
}

fn parse_inner(tokens: &mut TokenIter, in_statement: bool) -> Result<Vec<Node>, ParserError> {
    log::trace!("parse_inner");
    let mut ast = vec![];

    loop {
        match tokens.next() {
            Some((Token::Text(text), _)) => {
                ast.push(Node::Text(text.clone()));
            }
            Some((Token::OpenValue, _)) => {
                let (name, _) = extract_code(tokens)?;
                ast.push(Node::Identifier(name.clone()));
                consume_token(tokens, Token::CloseValue, false)?;
            }
            Some((Token::OpenBuilder, _)) => {
                let (name, _) = extract_code(tokens)?;
                ast.push(Node::Builder(name.clone()));
                consume_token(tokens, Token::CloseBuilder, false)?;
            }
            Some((Token::OpenStmt, _)) => {
                if let Some((Token::Else, _)) | Some((Token::EndIf, _)) | Some((Token::EndFor, _)) =
                    tokens.peek()
                {
                    if in_statement {
                        break;
                    } else {
                        match tokens.next() {
                            Some((token, range)) => {
                                return Err(ParserError::UnexpectedToken(
                                    token.clone(),
                                    range.clone(),
                                    vec![Token::If, Token::For],
                                ))
                            }
                            None => return Err(ParserError::UnexpectedEnd),
                        }
                    }
                }
                let node = parse_statement(tokens)?;
                ast.push(node);
            }
            Some((Token::OpenLine, _)) => {
                if let Some((Token::EndFn, _)) = tokens.peek() {
                    break;
                }

                match tokens.next() {
                    Some((Token::Import, _)) => {
                        let import_details = extract_import_details(tokens)?;
                        ast.push(Node::Import(import_details))
                    }
                    Some((Token::With, _)) => {
                        let (identifier, range) = extract_identifier(tokens)?;
                        consume_token(tokens, Token::As, false)?;
                        let (type_, _) = extract_identifier(tokens)?;
                        ast.push(Node::With((identifier, range), type_))
                    }
                    Some((Token::Fn, range)) => {
                        if in_statement {
                            return Err(ParserError::FunctionWithinStatement(range.clone()));
                        }

                        let node = parse_function(tokens, Visibility::Private)?;
                        ast.push(node);
                    }
                    Some((Token::Pub, pub_range)) => {
                        let fn_range = consume_token(tokens, Token::Fn, false)?;
                        let range = fn_range
                            .map(|range| Range {
                                start: std::cmp::min(range.start, pub_range.start),
                                end: std::cmp::max(range.end, pub_range.end),
                            })
                            .unwrap_or_else(|| pub_range.clone());
                        if in_statement {
                            return Err(ParserError::FunctionWithinStatement(range));
                        }
                        let node = parse_function(tokens, Visibility::Public)?;
                        ast.push(node);
                    }
                    _ => {}
                }
                consume_token(tokens, Token::CloseLine, false)?;
            }
            Some((token, range)) => {
                return Err(ParserError::UnexpectedToken(
                    token.clone(),
                    range.clone(),
                    vec![],
                ))
            }
            None => {
                break;
            }
        }
    }

    Ok(ast)
}

fn parse_function(tokens: &mut TokenIter, visibility: Visibility) -> Result<Node, ParserError> {
    let (head, range) = extract_code(tokens)?;
    consume_token(tokens, Token::CloseLine, false)?;
    let body = parse_inner(tokens, true)?;
    let body = trim_trailing_newline(body);
    consume_token(tokens, Token::EndFn, false)?;

    Ok(Node::BlockFunction(visibility, head, body, range))
}

fn parse_if_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    log::trace!("parse_if_statement");
    let (name, _) = extract_code(tokens)?;
    consume_token(tokens, Token::CloseStmt, false)?;

    let if_nodes = parse_inner(tokens, true)?;
    let mut else_nodes = vec![];

    match tokens.next() {
        Some((Token::EndIf, _)) => {
            consume_token(tokens, Token::CloseStmt, false)?;
        }
        Some((Token::Else, _)) => {
            consume_token(tokens, Token::CloseStmt, false)?;

            else_nodes = parse_inner(tokens, true)?;
            consume_token(tokens, Token::EndIf, false)?;
            consume_token(tokens, Token::CloseStmt, false)?;
        }
        Some((token, range)) => {
            return Err(ParserError::UnexpectedToken(
                token.clone(),
                range.clone(),
                vec![Token::EndIf, Token::Else],
            ));
        }
        None => {
            return Err(ParserError::UnexpectedEnd);
        }
    }

    Ok(Node::If(name, if_nodes, else_nodes))
}

fn parse_for_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    log::trace!("parse_for_statement");
    let (entry_identifier, _) = extract_identifier(tokens)?;
    let entry_type = match tokens.next() {
        Some((Token::As, _)) => {
            let (type_identifier, _) = extract_identifier(tokens)?;
            consume_token(tokens, Token::In, false)?;
            Some(type_identifier)
        }
        Some((Token::In, _)) => None,
        Some((matched_token, range)) => {
            return Err(ParserError::UnexpectedToken(
                matched_token.clone(),
                range.clone(),
                vec![Token::As, Token::In],
            ));
        }
        None => return Err(ParserError::UnexpectedEnd),
    };

    let (list_identifier, _) = extract_code(tokens)?;
    consume_token(tokens, Token::CloseStmt, false)?;

    let loop_nodes = parse_inner(tokens, true)?;

    consume_token(tokens, Token::EndFor, false)?;
    consume_token(tokens, Token::CloseStmt, false)?;

    Ok(Node::For(
        entry_identifier,
        entry_type,
        list_identifier,
        loop_nodes,
    ))
}

fn extract_identifier(tokens: &mut TokenIter) -> Result<(String, Range), ParserError> {
    log::trace!("extract_identifier");
    match tokens.next() {
        Some((Token::GleamTokenOrIdentifier(name), range)) => Ok((name.clone(), range.clone())),
        Some((token, range)) => Err(ParserError::UnexpectedToken(
            token.clone(),
            range.clone(),
            vec![Token::GleamTokenOrIdentifier("".to_string())],
        )),
        None => Err(ParserError::UnexpectedEnd),
    }
}

fn extract_code(tokens: &mut TokenIter) -> Result<(String, Range), ParserError> {
    log::trace!("extract_code");
    let mut code = String::new();
    let mut range: Option<Range> = None;

    loop {
        match tokens.peek() {
            Some((Token::GleamTokenOrIdentifier(name), token_range)) => {
                // Create range and expand it to include all the tokens
                // that we're adding to this string
                range = range
                    .map(|current| Range {
                        start: std::cmp::min(token_range.start, current.start),
                        end: std::cmp::max(token_range.end, current.end),
                    })
                    .and(Some(token_range.clone()));

                // Place a space between each identifier
                if !code.is_empty() {
                    code.push(' ');
                }

                code.push_str(name);
                tokens.next();
            }
            Some((Token::CloseStmt, _)) => break,
            Some((Token::CloseValue, _)) => break,
            Some((Token::CloseBuilder, _)) => break,
            Some((Token::CloseLine, _)) => break,
            Some((token, range)) => {
                if code.is_empty() {
                    return Err(ParserError::UnexpectedToken(
                        token.clone(),
                        range.clone(),
                        vec![Token::GleamTokenOrIdentifier("".to_string())],
                    ));
                } else {
                    break;
                }
            }
            None => return Err(ParserError::UnexpectedEnd),
        }
    }

    Ok((code, range.unwrap_or(Range { start: 0, end: 0 })))
}

fn extract_import_details(tokens: &mut TokenIter) -> Result<String, ParserError> {
    log::trace!("extract_import_details");
    match tokens.next() {
        Some((Token::ImportDetails(details), _)) => Ok(details.clone()),
        Some((token, range)) => Err(ParserError::UnexpectedToken(
            token.clone(),
            range.clone(),
            vec![Token::ImportDetails("".to_string())],
        )),
        None => Err(ParserError::UnexpectedEnd),
    }
}

fn consume_token(
    tokens: &mut TokenIter,
    expected_token: Token,
    accept_end: bool,
) -> Result<Option<Range>, ParserError> {
    log::trace!("consume_token: {:?}", expected_token);
    match tokens.next() {
        Some((matched_token, range)) if *matched_token == expected_token => Ok(Some(range.clone())),
        Some((matched_token, range)) => Err(ParserError::UnexpectedToken(
            matched_token.clone(),
            range.clone(),
            vec![expected_token],
        )),
        None => {
            if accept_end {
                Ok(None)
            } else {
                log::error!(
                    "consume_token - found: None, expected_token: {:?}",
                    expected_token
                );
                Err(ParserError::UnexpectedEnd)
            }
        }
    }
}

/// Find the last item in the nodes and if it is a Text node then trim the final '\n' from it. If
/// it is just a '\n' then drop the node entirely
fn trim_trailing_newline(nodes: Vec<Node>) -> Vec<Node> {
    let length = nodes.len();
    nodes
        .into_iter()
        .enumerate()
        .flat_map(|(i, node)| {
            if i == length - 1 {
                match node {
                    Node::Text(text) => {
                        if text == "\n" {
                            None
                        } else {
                            Some(Node::Text(
                                text.strip_suffix('\n')
                                    .map(String::from)
                                    .unwrap_or_else(|| text.to_string()),
                            ))
                        }
                    }

                    node => Some(node),
                }
            } else {
                Some(node)
            }
        })
        .collect()
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use crate::scanner::{self, ScanError};

    use super::*;

    // Our only use of this is the Debug output but the compiler's dead code analysis ignores
    // Debug usage so we have to allow dead_code here
    #[derive(Debug)]
    #[expect(dead_code)]
    pub enum Error {
        Scan(ScanError),
        Parse(ParserError),
    }

    fn format_debug_result<T: Debug, E: Debug>(result: Result<T, E>) -> String {
        match result {
            Ok(value) => format!("{:#?}", value),
            Err(err) => format!("{:#?}", err),
        }
    }

    #[macro_export]
    macro_rules! assert_parse {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let result = scanner::scan($text)
                .map_err(|err| Error::Scan(err))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable()).map_err(|err| Error::Parse(err))
                });
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format_debug_result(result),
                $text
            );
        }};
    }

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
    fn test_parse_gleam_expression() {
        assert_parse!("Hello {{ string.uppercase(name) }}, good to meet you");
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
    fn test_parse_if_comparison() {
        assert_parse!("Hello {% if items != [] %}Some items{% endif %}");
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
    fn test_parse_for_as_loop() {
        assert_parse!(
            "Hello,{% for item as Item in list %} to {{ item }} and {% endfor %} everyone else"
        );
    }

    #[test]
    fn test_parse_for_from_expression() {
        assert_parse!("Hello {% for item as Item in list.take(list, 2) %}{{ item }}{% endfor %}");
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

    #[test]
    fn test_parse_tree_block() {
        assert_parse!("Hello {[ name ]}, good to meet you");
    }

    #[test]
    fn test_parse_tree_expression() {
        assert_parse!("Hello {[ string_tree.from_strings([\"Anna\", \" and \", \"Bob\"]) ]}, good to meet you");
    }

    #[test]
    fn test_parse_function() {
        assert_parse!("{> fn classes()\na b c d\n{> endfn\n");
    }

    #[test]
    fn test_parse_function_with_trailing_new_line() {
        assert_parse!("{> fn classes()\na b c d\n\n{> endfn\n");
    }

    #[test]
    fn test_parse_public_function() {
        assert_parse!("{> pub fn classes()\na b c d\n{> endfn\n");
    }

    #[test]
    fn test_parse_function_with_arg_and_usage() {
        assert_parse!(
            r#"{> fn full_name(second_name: String)
Lucy {{ second_name }}
{> endfn
Hello {[ full_name("Gleam") ]}"#
        );
    }
}
