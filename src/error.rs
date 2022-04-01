use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor;

use crate::parser::ParserError;
use crate::renderer::RenderError;
use crate::scanner::{Range, ScanError, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Source {
    pub filename: String,
    pub contents: String,
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error, std::path::PathBuf),
    Scan(ScanError, Source),
    Parse(ParserError, Source),
    Render(RenderError, Source),
}

pub fn write<W: termcolor::WriteColor>(writer: &mut W, error: Error) {
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
                Token::GleamToken(name) => explain_with_source(
                    writer,
                    &format!("Unexpected token: {}", name),
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

#[cfg(test)]
mod test {
    use codespan_reporting::term::termcolor::Buffer;

    use super::*;

    use crate::parser;
    use crate::renderer;
    use crate::scanner;

    fn error_to_string(error: Error) -> String {
        let mut writer = Buffer::no_color();
        write(&mut writer, error);

        std::str::from_utf8(writer.as_slice())
            .unwrap_or("Failure")
            .to_string()
    }

    fn format_result(result: Result<String, Error>) -> String {
        match result {
            Ok(value) => value,
            Err(err) => error_to_string(err),
        }
    }

    const NAME: &str = env!("CARGO_PKG_NAME");

    #[macro_export]
    macro_rules! assert_error {
        ($text:expr $(,)?) => {{
            let _ = env_logger::try_init();
            let source = Source {
                filename: "-test-".to_string(),
                contents: $text.to_string(),
            };
            let result = scanner::scan($text)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parser::parse(&mut tokens.iter().peekable())
                        .map_err(|err| Error::Parse(err, source.clone()))
                })
                .and_then(|ast| {
                    renderer::render(&mut ast.iter().peekable(), NAME, "-test-")
                        .map_err(|err| Error::Render(err, source.clone()))
                });
            insta::assert_snapshot!(insta::internals::AutoName, format_result(result), $text);
        }};
    }

    #[test]
    fn test_error_unknown_keyword() {
        assert_error!(r#"Hello {% wrong %}"#);
    }

    #[test]
    fn test_error_unexpected_keyword() {
        assert_error!(r#"Hello {% in %}"#);
    }

    #[test]
    fn test_error_unexpected_endif() {
        assert_error!(r#"Hello {% endif %}"#);
    }

    #[test]
    fn test_error_duplicate_with() {
        assert_error!(
            r#"{> with name as String
{> with name as String
Hello"#
        );
    }

    #[test]
    fn test_error_duplicate_with_name() {
        assert_error!(
            r#"{> with name as String
{> with name as String
Hello"#
        );
    }
}
