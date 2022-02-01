use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{self, ColorChoice, StandardStream};
use structopt::StructOpt;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use walkdir::WalkDir;

#[derive(Debug, PartialEq, Clone)]
pub struct Source {
    pub filename: String,
    pub contents: String,
}

pub type Range = std::ops::Range<usize>;
pub type Position = usize;

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
enum ScanError {
    UnexpectedGrapheme(String, Position),
    UnexpectedEnd,
}

type Tokens = Vec<(Token, Range)>;

fn scan(contents: &str) -> Result<Tokens, ScanError> {
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

// Parsing

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, (Token, Range)>>;

type Type = String;

#[derive(Debug)]
enum Node {
    Text(String),
    Identifier(String),
    Builder(String),
    If(String, Vec<Node>, Vec<Node>),
    For(String, Option<Type>, String, Vec<Node>),
    Import(String),
    With(String, Type),
}

#[derive(Debug)]
enum ParserError {
    UnexpectedToken(Token, Range, Vec<Token>),
    UnexpectedEnd,
}

fn parse(tokens: &mut TokenIter) -> Result<Vec<Node>, ParserError> {
    log::trace!("parse");
    parse_inner(tokens, false)
}

fn parse_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
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
                let name = extract_identifier(tokens)?;
                ast.push(Node::Identifier(name.clone()));
                consume_token(tokens, Token::CloseValue)?;
            }
            Some((Token::OpenBuilder, _)) => {
                let name = extract_identifier(tokens)?;
                ast.push(Node::Builder(name.clone()));
                consume_token(tokens, Token::CloseBuilder)?;
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
                match tokens.next() {
                    Some((Token::Import, _)) => {
                        let import_details = extract_import_details(tokens)?;
                        ast.push(Node::Import(import_details))
                    }
                    Some((Token::With, _)) => {
                        let identifier = extract_identifier(tokens)?;
                        consume_token(tokens, Token::As)?;
                        let type_ = extract_identifier(tokens)?;
                        ast.push(Node::With(identifier, type_))
                    }
                    _ => {}
                }
                consume_token(tokens, Token::CloseLine)?;
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

fn parse_if_statement(tokens: &mut TokenIter) -> Result<Node, ParserError> {
    log::trace!("parse_if_statement");
    let name = extract_identifier(tokens)?;
    consume_token(tokens, Token::CloseStmt)?;

    let if_nodes = parse_inner(tokens, true)?;
    let mut else_nodes = vec![];

    match tokens.next() {
        Some((Token::EndIf, _)) => {
            consume_token(tokens, Token::CloseStmt)?;
        }
        Some((Token::Else, _)) => {
            consume_token(tokens, Token::CloseStmt)?;

            else_nodes = parse_inner(tokens, true)?;
            consume_token(tokens, Token::EndIf)?;
            consume_token(tokens, Token::CloseStmt)?;
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
    let entry_identifier = extract_identifier(tokens)?;
    let entry_type = match tokens.next() {
        Some((Token::As, _)) => {
            let type_identifier = extract_identifier(tokens)?;
            consume_token(tokens, Token::In)?;
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

    let list_identifier = extract_identifier(tokens)?;
    consume_token(tokens, Token::CloseStmt)?;

    let loop_nodes = parse_inner(tokens, true)?;

    consume_token(tokens, Token::EndFor)?;
    consume_token(tokens, Token::CloseStmt)?;

    Ok(Node::For(
        entry_identifier,
        entry_type,
        list_identifier,
        loop_nodes,
    ))
}

fn extract_identifier(tokens: &mut TokenIter) -> Result<String, ParserError> {
    log::trace!("extract_identifier");
    match tokens.next() {
        Some((Token::Identifier(name), _)) => Ok(name.clone()),
        Some((token, range)) => Err(ParserError::UnexpectedToken(
            token.clone(),
            range.clone(),
            vec![Token::Identifier("".to_string())],
        )),
        None => Err(ParserError::UnexpectedEnd),
    }
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

fn consume_token(tokens: &mut TokenIter, expected_token: Token) -> Result<(), ParserError> {
    log::trace!("consume_token");
    match tokens.next() {
        Some((matched_token, _)) if *matched_token == expected_token => Ok(()),
        Some((matched_token, range)) => Err(ParserError::UnexpectedToken(
            matched_token.clone(),
            range.clone(),
            vec![expected_token.clone()],
        )),
        None => Err(ParserError::UnexpectedEnd),
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
        .flat_map(|value| value.split('.').next())
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
                    text.replace("\"", "\\\"")
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
            Some(Node::Builder(name)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append_builder(builder, {})\n",
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
            Some(Node::For(entry_identifier, entry_type, list_identifier, loop_nodes)) => {
                iter.next();

                let entry_type = entry_type
                    .as_ref()
                    .map(|value| format!(": {}", value))
                    .unwrap_or("".to_string());

                let (loop_lines, mut loop_params, _, _) =
                    render_lines(&mut loop_nodes.iter().peekable());
                builder_lines.push_str(&format!(
                    r#"    let builder = list.fold({}, builder, fn(builder, {}{}) {{
        {}
        builder
}})
"#,
                    list_identifier, entry_identifier, entry_type, loop_lines
                ));
                params.push(list_identifier.clone());

                // Remove the for-loop identifier which will have been detected as a 'param'
                // We split any discovered identifiers on '.' and compare the first part so that if
                // the entry_identifier is 'user' and we find 'user.name' we still rule it out
                loop_params.retain(|value| value.split(".").next() != Some(entry_identifier));
                params.append(&mut loop_params);
            }
            None => break,
        }
    }

    (builder_lines, params, imports, type_lookup)
}

#[derive(Debug)]
enum Error {
    IO(std::io::Error, std::path::PathBuf),
    Scan(ScanError, Source),
    Parse(ParserError, Source),
}

fn write_error<W: termcolor::WriteColor>(writer: &mut W, error: Error) {
    match error {
        Error::IO(error, filepath) => match error.kind() {
            std::io::ErrorKind::NotFound => {
                let _ = write!(writer, "File not found: {}", filepath.to_string_lossy());
            }
            std::io::ErrorKind::PermissionDenied => {
                let _ = write!(writer, "Permission denied: {}", filepath.to_string_lossy());
            }
            _ => {
                let _ = write!(writer, "Unknown IO Error: {}", filepath.to_string_lossy());
            }
        },
        Error::Scan(error, source) => match error {
            ScanError::UnexpectedGrapheme(grapheme, position) => {
                let range = Range {
                    start: position,
                    end: position,
                };
                explain_with_source(
                    writer,
                    &format!("Unexpected character: {}", grapheme),
                    source,
                    range,
                );
            }
            ScanError::UnexpectedEnd => {
                let _ = write!(writer, "Unexpected end");
            }
        },
        Error::Parse(error, source) => match error {
            ParserError::UnexpectedToken(token, range, expected) => match token {
                Token::Identifier(name) => explain_with_source(
                    writer,
                    &format!("Unexpected identifier: {}", name),
                    source,
                    range,
                ),

                Token::Import
                | Token::With
                | Token::As
                | Token::OpenStmt
                | Token::CloseStmt
                | Token::If
                | Token::Else
                | Token::EndIf
                | Token::For
                | Token::EndFor
                | Token::In => {
                    if expected.is_empty() {
                        explain_with_source(
                            writer,
                            &format!("Unexpected keyword: {}", token),
                            source,
                            range,
                        )
                    } else {
                        explain_with_source(
                            writer,
                            &format!(
                                "Unexpected keyword: {}. Expected one of: {}",
                                token,
                                expected
                                    .iter()
                                    .map(|token| format!("{}", token))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            source,
                            range,
                        )
                    }
                }

                _ => explain_with_source(
                    writer,
                    &format!("Unexpected token: {:?}", token),
                    source,
                    range,
                ),
            },
            ParserError::UnexpectedEnd => {
                let _ = write!(writer, "Unexpected end");
            }
        },
    }
}

fn explain_with_source<W: termcolor::WriteColor>(
    writer: &mut W,
    text: &str,
    source: Source,
    range: Range,
) {
    let _ = write!(
        writer,
        r#"{}

"#,
        text,
    );
    pretty_print(writer, source, range);
}

fn pretty_print<W: termcolor::WriteColor>(writer: &mut W, source: Source, range: Range) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(source.filename, source.contents);
    let diagnostic = Diagnostic::error().with_labels(vec![Label::primary(file_id, range)]);

    let config = codespan_reporting::term::Config::default();

    let _ = term::emit(writer, &config, &files, &diagnostic);
}

fn convert(filepath: &std::path::Path) {
    let result = std::fs::read_to_string(filepath)
        .map_err(|err| Error::IO(err, filepath.to_path_buf()))
        .and_then(|contents| {
            let source = Source {
                filename: filepath.to_string_lossy().into_owned(),
                contents: contents.clone(),
            };
            scan(&contents)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|error| Error::Parse(error, source.clone()))
                })
        })
        .map(|ast| render(&mut ast.iter().peekable()))
        .and_then(|output| {
            let out_file_path = filepath.with_extension("gleam");
            std::fs::write(&out_file_path, output)
                .map_err(|err| Error::IO(err, out_file_path.to_path_buf()))
        });

    match result {
        Ok(()) => {}
        Err(error) => {
            let mut writer = StandardStream::stderr(color_choice());
            write_error(&mut writer, error);
        }
    };
}

fn color_choice() -> ColorChoice {
    if atty::is(atty::Stream::Stderr) {
        termcolor::ColorChoice::Auto
    } else {
        termcolor::ColorChoice::Never
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "templates", about = "Compiles templates into Gleam modules")]
struct Opt {
    #[structopt(short, long)]
    verbose: bool,

    #[structopt(long)]
    version: bool,
}

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let opt = Opt::from_args();
    if opt.version {
        println!("{}", VERSION);
        return;
    }

    for entry in WalkDir::new(".").into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        if path.extension() == Some(std::ffi::OsStr::new("gleamx")) {
            if opt.verbose {
                println!("Converting {}", path.display());
            }
            convert(&path.to_path_buf());
        }
    }
}

#[cfg(test)]
mod test {
    use codespan_reporting::term::termcolor::Buffer;

    use super::*;

    fn error_to_string(error: Error) -> String {
        let mut writer = Buffer::no_color();
        write_error(&mut writer, error);

        std::str::from_utf8(writer.as_slice())
            .unwrap_or("Failure")
            .to_string()
    }

    fn debug_format_result<T: Debug, E: Debug>(result: Result<T, E>) -> String {
        match result {
            Ok(value) => format!("{:#?}", value),
            Err(err) => format!("{:#?}", err),
        }
    }

    fn format_result(result: Result<String, Error>) -> String {
        match result {
            Ok(value) => value,
            Err(err) => error_to_string(err),
        }
    }

    #[macro_export]
    macro_rules! assert_scan {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            insta::assert_snapshot!(
                insta::internals::AutoName,
                debug_format_result(scan($text)),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_parse {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let source = Source {
                filename: "-test-".to_string(),
                contents: $text.to_string(),
            };
            let result = scan($text)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|err| Error::Parse(err, source.clone()))
                });
            insta::assert_snapshot!(
                insta::internals::AutoName,
                debug_format_result(result),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_render {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let source = Source {
                filename: "-test-".to_string(),
                contents: $text.to_string(),
            };
            let result = scan($text)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|err| Error::Parse(err, source.clone()))
                })
                .map(|ast| render(&mut ast.iter().peekable()));
            insta::assert_snapshot!(insta::internals::AutoName, format_result(result), $text);
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
    fn test_parse_for_as_loop() {
        assert_parse!(
            "Hello,{% for item as Item in list %} to {{ item }} and {% endfor %} everyone else"
        );
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
    fn test_parse_builder_block() {
        assert_parse!("Hello {[ name ]}, good to meet you");
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
    fn test_render_for_as_loop() {
        assert_render!(
            "Hello,{% for item as Item in list %} to {{ item }} and {% endfor %} everyone else"
        );
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

    #[test]
    fn test_render_multiline() {
        assert_render!(
            r#"<ul>
{% for entry in my_list %}
    <li>{{ entry }}</li>
{% endfor %}
</ul>"#
        );
    }

    #[test]
    fn test_render_quotes() {
        assert_render!(r#"<div class="my-class">{{ name }}</div>"#);
    }

    #[test]
    fn test_render_builder_block() {
        assert_render!("Hello {[ name ]}, good to meet you");
    }

    // Errors

    #[test]
    fn test_error_unknown_keyword() {
        assert_render!(r#"Hello {% wrong %}"#);
    }

    #[test]
    fn test_error_unexpected_keyword() {
        assert_render!(r#"Hello {% in %}"#);
    }

    #[test]
    fn test_error_unexpected_endif() {
        assert_render!(r#"Hello {% endif %}"#);
    }
}
