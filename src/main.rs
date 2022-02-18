use std::fmt::Debug;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{self, ColorChoice, StandardStream};
use structopt::StructOpt;
use walkdir::WalkDir;

mod scanner;

use scanner::{Range, ScanError, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Source {
    pub filename: String,
    pub contents: String,
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
    With((String, Range), Type),
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
                let (name, _) = extract_identifier(tokens)?;
                ast.push(Node::Identifier(name.clone()));
                consume_token(tokens, Token::CloseValue)?;
            }
            Some((Token::OpenBuilder, _)) => {
                let (name, _) = extract_identifier(tokens)?;
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
                        let (identifier, range) = extract_identifier(tokens)?;
                        consume_token(tokens, Token::As)?;
                        let (type_, _) = extract_identifier(tokens)?;
                        ast.push(Node::With((identifier, range), type_))
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
    let (name, _) = extract_identifier(tokens)?;
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
    let (entry_identifier, _) = extract_identifier(tokens)?;
    let entry_type = match tokens.next() {
        Some((Token::As, _)) => {
            let (type_identifier, _) = extract_identifier(tokens)?;
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

    let (list_identifier, _) = extract_identifier(tokens)?;
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

fn extract_identifier(tokens: &mut TokenIter) -> Result<(String, Range), ParserError> {
    log::trace!("extract_identifier");
    match tokens.next() {
        Some((Token::Identifier(name), range)) => Ok((name.clone(), range.clone())),
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
            vec![expected_token],
        )),
        None => Err(ParserError::UnexpectedEnd),
    }
}

// Rendering

type NodeIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Node>>;

#[derive(Debug)]
enum RenderError {
    DuplicateParamName(String, Range),
}

fn render(iter: &mut NodeIter) -> Result<String, RenderError> {
    let (builder_lines, imports, typed_params) = render_lines(iter)?;

    let import_lines = imports
        .iter()
        .map(|details| format!("import {}", details))
        .collect::<Vec<_>>()
        .join("\n");

    let params_string = typed_params
        .iter()
        .map(|(param_name, type_name)| format!("{} {}: {}", param_name, param_name, type_name))
        .collect::<Vec<_>>()
        .join(", ");

    let args_string = typed_params
        .iter()
        .map(|(param_name, _)| format!("{}: {}", param_name, param_name))
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

    Ok(output)
}

type RenderDetails = (String, Vec<String>, Vec<(String, String)>);

fn render_lines(iter: &mut NodeIter) -> Result<RenderDetails, RenderError> {
    let mut builder_lines = String::new();
    let mut imports = vec![];

    // Use a Vec<(String, String)> instead of a HashMap to maintain order which gives the users
    // some control, though parameters are labelled and can be called in any order. Some kind of
    // order is required to keep the tests passing as it seems to be non-determinate in a HashMap
    let mut typed_params = Vec::new();

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
            }
            Some(Node::Builder(name)) => {
                iter.next();
                builder_lines.push_str(&format!(
                    "    let builder = string_builder.append_builder(builder, {})\n",
                    name
                ));
            }
            Some(Node::Import(import_details)) => {
                iter.next();
                imports.push(import_details.clone());
            }
            Some(Node::With((identifier, range), type_)) => {
                iter.next();

                if typed_params.iter().any(|(name, _)| name == identifier) {
                    return Err(RenderError::DuplicateParamName(
                        identifier.clone(),
                        range.clone(),
                    ));
                }

                typed_params.push((identifier.clone(), type_.clone()));
            }
            Some(Node::If(identifier_name, if_nodes, else_nodes)) => {
                iter.next();
                let (if_lines, _, _) = render_lines(&mut if_nodes.iter().peekable())?;
                let (else_lines, _, _) = render_lines(&mut else_nodes.iter().peekable())?;
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
            }
            Some(Node::For(entry_identifier, entry_type, list_identifier, loop_nodes)) => {
                iter.next();

                let entry_type = entry_type
                    .as_ref()
                    .map(|value| format!(": {}", value))
                    .unwrap_or_else(|| "".to_string());

                let (loop_lines, _, _) = render_lines(&mut loop_nodes.iter().peekable())?;
                builder_lines.push_str(&format!(
                    r#"    let builder = list.fold({}, builder, fn(builder, {}{}) {{
        {}
        builder
}})
"#,
                    list_identifier, entry_identifier, entry_type, loop_lines
                ));
            }
            None => break,
        }
    }

    Ok((builder_lines, imports, typed_params))
}

#[derive(Debug)]
enum Error {
    IO(std::io::Error, std::path::PathBuf),
    Scan(ScanError, Source),
    Parse(ParserError, Source),
    Render(RenderError, Source),
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
        Error::Render(error, source) => match error {
            RenderError::DuplicateParamName(name, range) => explain_with_source(
                writer,
                &format!(
                    "This is declared in more than one 'with' statement: {}",
                    name
                ),
                source,
                range,
            ),
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
            scanner::scan(&contents)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|error| Error::Parse(error, source.clone()))
                })
                .and_then(|ast| {
                    render(&mut ast.iter().peekable())
                        .map_err(|error| Error::Render(error, source.clone()))
                })
        })
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

    fn format_debug_result<T: Debug, E: Debug>(result: Result<T, E>) -> String {
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
    macro_rules! assert_parse {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let source = Source {
                filename: "-test-".to_string(),
                contents: $text.to_string(),
            };
            let result = scanner::scan($text)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|err| Error::Parse(err, source.clone()))
                });
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format_debug_result(result),
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
            let result = scanner::scan($text)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parse(&mut tokens.iter().peekable())
                        .map_err(|err| Error::Parse(err, source.clone()))
                })
                .and_then(|ast| {
                    render(&mut ast.iter().peekable())
                        .map_err(|err| Error::Render(err, source.clone()))
                });
            insta::assert_snapshot!(insta::internals::AutoName, format_result(result), $text);
        }};
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
        assert_render!(
            "{> with name as String
Hello {{ name }}, good to meet you"
        );
    }

    #[test]
    fn test_render_two_identifiers() {
        assert_render!(
            "{> with name as String
{> with adjective as String
Hello {{ name }}, {{ adjective }} to meet you"
        );
    }

    #[test]
    fn test_repeated_identifier_usage() {
        assert_render!(
            "{> with name as String
{{ name }} usage, {{ name }} usage"
        );
    }

    #[test]
    fn test_render_if_statement() {
        assert_render!(
            "{> with is_user as Bool
Hello {% if is_user %}User{% endif %}"
        );
    }

    #[test]
    fn test_render_empty_if_statement() {
        assert_render!(
            "{> with is_user as Bool
Hello {% if is_user %}{% endif %}"
        );
    }

    #[test]
    fn test_render_if_else_statement() {
        assert_render!(
            "{> with is_user as Bool
Hello {% if is_user %}User{% else %}Unknown{% endif %}"
        );
    }

    #[test]
    fn test_render_nested_if_statements() {
        assert_render!(
            "{> with is_user as Bool
{> with is_admin as Bool
Hello {% if is_user %}{% if is_admin %}Admin{% else %}User{% endif %}{% endif %}"
        );
    }

    #[test]
    fn test_render_for_loop() {
        assert_render!(
            "{> with list as List(String)
Hello,{% for item in list %} to {{ item }} and {% endfor %} everyone else"
        );
    }

    #[test]
    fn test_render_for_as_loop() {
        assert_render!(
            "{> with list as List(Item)
Hello,{% for item as Item in list %} to {{ item }} and {% endfor %} everyone else"
        );
    }

    #[test]
    fn test_render_dot_access() {
        assert_render!(
            "{> with user as MyUser
Hello{% if user.is_admin %} Admin{% endif %}"
        );
    }

    #[test]
    fn test_render_import() {
        assert_render!("{> import user.{User}\n{> with name as String\n{{ name }}");
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
            r#"{> with my_list as List(String)
<ul>
{% for entry in my_list %}
    <li>{{ entry }}</li>
{% endfor %}
</ul>"#
        );
    }

    #[test]
    fn test_render_quotes() {
        assert_render!(
            r#"{> with name as String
<div class="my-class">{{ name }}</div>"#
        );
    }

    #[test]
    fn test_render_builder_block() {
        assert_render!(
            "{> with name as StringBuilder
Hello {[ name ]}, good to meet you"
        );
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

    #[test]
    fn test_error_duplicate_with() {
        assert_render!(
            r#"{> with name as String
{> with name as String
Hello"#
        );
    }

    #[test]
    fn test_error_duplicate_with_name() {
        assert_render!(
            r#"{> with name as String
{> with name as String
Hello"#
        );
    }
}
