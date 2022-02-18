use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

pub type Range = std::ops::Range<usize>;
pub type Position = usize;

type Iter<'a> = std::iter::Peekable<GraphemeIndices<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Text(String),
    OpenLine,
    CloseLine,
    OpenValue,
    CloseValue,
    OpenBuilder,
    CloseBuilder,
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Token::Text(string) => string,
            Token::OpenLine => "{>",
            Token::CloseLine => "\n",
            Token::OpenValue => "{{",
            Token::CloseValue => "}}",
            Token::OpenBuilder => "{[",
            Token::CloseBuilder => "]}",
            Token::OpenStmt => "{%",
            Token::CloseStmt => "%}",
            Token::Identifier(name) => name,
            Token::Import => "import",
            Token::ImportDetails(_) => "import-details",
            Token::With => "with",
            Token::As => "as",
            Token::If => "if",
            Token::Else => "else",
            Token::EndIf => "endif",
            Token::For => "for",
            Token::EndFor => "endfor",
            Token::In => "in",
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug)]
pub enum ScanError {
    UnexpectedGrapheme(String, Position),
    UnexpectedEnd,
}

type Tokens = Vec<(Token, Range)>;

pub fn scan(contents: &str) -> Result<Tokens, ScanError> {
    log::trace!("scan");
    let iter = contents.grapheme_indices(true);

    let tokens = scan_plain(&mut iter.peekable(), vec![])?;

    Ok(tokens)
}

fn scan_plain(iter: &mut Iter, mut tokens: Tokens) -> Result<Tokens, ScanError> {
    log::trace!("scan_plain");
    let mut buffer = String::new();
    let mut buffer_start_index = None;
    let mut buffer_end_index = 0;
    loop {
        match iter.next() {
            Some((first_index, "{")) => {
                if let Some((second_index, "{")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push((
                            Token::Text(buffer),
                            Range {
                                start: buffer_start_index.unwrap_or(0),
                                end: first_index,
                            },
                        ));
                        buffer = String::new();
                    }

                    tokens.push((
                        Token::OpenValue,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    iter.next();

                    tokens = scan_identifiers(iter, tokens)?;
                } else if let Some((second_index, "%")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push((
                            Token::Text(buffer),
                            Range {
                                start: buffer_start_index.unwrap_or(0),
                                end: first_index,
                            },
                        ));
                        buffer = String::new();
                    }

                    tokens.push((
                        Token::OpenStmt,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    iter.next();

                    tokens = scan_identifiers(iter, tokens)?;
                } else if let Some((second_index, "[")) = iter.peek() {
                    if !buffer.is_empty() {
                        tokens.push((
                            Token::Text(buffer),
                            Range {
                                start: buffer_start_index.unwrap_or(0),
                                end: first_index,
                            },
                        ));
                        buffer = String::new();
                    }

                    tokens.push((
                        Token::OpenBuilder,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));

                    iter.next();

                    tokens = scan_identifiers(iter, tokens)?;
                } else if let Some((second_index, ">")) = iter.peek() {
                    tokens.push((
                        Token::OpenLine,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    iter.next();

                    tokens = scan_line(iter, tokens)?;

                    let range = consume_grapheme(iter, "\n")?;
                    tokens.push((Token::CloseLine, range));
                } else {
                    buffer.push('{');
                }
            }
            Some((first_index, "}")) => {
                if let Some((second_index, "}")) = iter.peek() {
                    tokens.push((
                        Token::CloseValue,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    buffer_start_index = None;
                    iter.next();
                } else {
                    buffer.push('}');
                }
            }
            Some((first_index, "%")) => {
                if let Some((second_index, "}")) = iter.peek() {
                    tokens.push((
                        Token::CloseStmt,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    buffer_start_index = None;
                    iter.next();
                } else {
                    buffer.push('%');
                }
            }
            Some((first_index, "]")) => {
                if let Some((second_index, "}")) = iter.peek() {
                    tokens.push((
                        Token::CloseBuilder,
                        Range {
                            start: first_index,
                            end: *second_index,
                        },
                    ));
                    buffer_start_index = None;
                    iter.next();
                } else {
                    buffer.push(']');
                }
            }
            Some((index, grapheme)) => {
                buffer.push_str(grapheme);
                buffer_start_index = buffer_start_index.or(Some(index));
                buffer_end_index = index;
            }
            None => {
                if !buffer.is_empty() {
                    tokens.push((
                        Token::Text(buffer),
                        Range {
                            start: buffer_start_index.unwrap_or(0),
                            end: buffer_end_index,
                        },
                    ));
                }
                return Ok(tokens);
            }
        }
    }
}

fn scan_identifiers(iter: &mut Iter, mut tokens: Tokens) -> Result<Tokens, ScanError> {
    log::trace!("scan_identifiers");
    loop {
        match iter.peek() {
            Some((_index, "}")) | Some((_index, "%")) | Some((_index, "]"))
            | Some((_index, "\n")) => {
                break;
            }
            None => break,
            Some(_) => {
                eat_spaces(iter);
                let (identifier_or_keyword, range) = scan_identifier_or_keyword(iter);
                tokens.push((identifier_or_keyword, range));
                eat_spaces(iter);
            }
        }
    }

    Ok(tokens)
}

fn scan_identifier_or_keyword(iter: &mut Iter) -> (Token, Range) {
    log::trace!("scan_identifier_or_keyword");
    let mut name = String::new();
    let mut start = None;
    let mut end = 0;

    loop {
        match iter.peek() {
            Some((_index, "}")) | Some((_index, " ")) | Some((_index, "\n"))
            | Some((_index, "%")) | Some((_index, "]")) => {
                break;
            }
            Some((index, grapheme)) => {
                start = start.or(Some(*index));
                end = *index + 1;
                name.push_str(grapheme);
                iter.next();
            }
            None => {
                break;
            }
        }
    }

    (
        to_token(name.trim()),
        Range {
            start: start.unwrap_or(0),
            end,
        },
    )
}

fn to_token(identifier: &str) -> Token {
    log::trace!("to_token: {}", identifier);
    match identifier {
        "if" => Token::If,
        "else" => Token::Else,
        "endif" => Token::EndIf,
        "for" => Token::For,
        "endfor" => Token::EndFor,
        "in" => Token::In,
        "import" => Token::Import,
        "with" => Token::With,
        "as" => Token::As,
        other => Token::Identifier(other.to_string()),
    }
}

fn scan_line(iter: &mut Iter, mut tokens: Tokens) -> Result<Tokens, ScanError> {
    eat_spaces(iter);

    let (token, range) = scan_identifier_or_keyword(iter);
    tokens.push((token.clone(), range));

    eat_spaces(iter);

    match token {
        Token::Import => {
            let (import_details, range) = scan_import_details(iter);
            tokens.push((Token::ImportDetails(import_details), range));
        }
        _ => {
            tokens = scan_identifiers(iter, tokens)?;
        }
    }

    Ok(tokens)
}

fn scan_import_details(iter: &mut Iter) -> (String, Range) {
    log::trace!("scan_import_details");
    let mut details = String::new();
    let mut start = None;
    let mut end = 0;

    loop {
        match iter.peek() {
            Some((_index, "\n")) => {
                break;
            }
            Some((index, grapheme)) => {
                start = start.or(Some(*index));
                end = *index + 1;

                details.push_str(grapheme);
                iter.next();
            }
            None => {
                break;
            }
        }
    }

    (
        details,
        Range {
            start: start.unwrap_or(0),
            end,
        },
    )
}

fn consume_grapheme(iter: &mut Iter, expected: &str) -> Result<Range, ScanError> {
    log::trace!("consume_grapheme");
    match iter.next() {
        Some((index, grapheme)) if grapheme == expected => Ok(Range {
            start: index,
            end: index,
        }),
        entry => Err(entry
            .map(|(index, value)| ScanError::UnexpectedGrapheme(value.to_string(), index))
            .unwrap_or(ScanError::UnexpectedEnd)),
    }
}

fn eat_spaces(iter: &mut Iter) {
    log::trace!("eat_spaces");
    while let Some((_index, " ")) = iter.peek() {
        iter.next();
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use super::*;

    fn format_debug_result<T: Debug, E: Debug>(result: Result<T, E>) -> String {
        match result {
            Ok(value) => format!("{:#?}", value),
            Err(err) => format!("{:#?}", err),
        }
    }

    #[macro_export]
    macro_rules! assert_scan {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format_debug_result(scan($text)),
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
    fn test_scan_for_as_loop() {
        assert_scan!("Hello {% for item as Item in list %}{{ item }}{% endfor %}");
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

    #[test]
    fn test_scan_builder_block() {
        assert_scan!("Hello {[ builder ]}, good to meet you");
    }
}
