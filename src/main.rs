use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};
use walkdir::WalkDir;

type Iter<'a> = std::iter::Peekable<GraphemeIndices<'a>>;

#[derive(Debug)]
enum Token {
    Text(String),
    OpenParen,
    CloseParen,
    Reference(String),
}

fn scan(contents: &str) -> Vec<Token> {
    let iter = contents.grapheme_indices(true);
    let mut tokens = vec![];

    scan_plain(&mut iter.peekable(), &mut tokens);

    tokens
}

fn scan_plain(iter: &mut Iter, tokens: &mut Vec<Token>) {
    let mut buffer = String::new();
    loop {
        match iter.peek() {
            Some((_index, "{")) => {
                iter.next();

                // Check for escaped '{'
                if let Some((_, "{")) = iter.peek() {
                    buffer.push_str("{");
                    iter.next();
                    continue;
                }

                tokens.push(Token::Text(buffer));
                tokens.push(Token::OpenParen);
                buffer = String::new();
                eat_white_space(iter);
                let identifier = scan_identifier(iter);
                tokens.push(Token::Reference(identifier));
                eat_white_space(iter);

                // Consume "}"
                tokens.push(Token::CloseParen);
                iter.next();
            }
            Some((_index, "}")) => {
                iter.next();

                // Check for escaped '}'
                if let Some((_, "}")) = iter.peek() {
                    buffer.push_str("}");
                    iter.next();
                    continue;
                }
            }
            Some((_index, grapheme)) => {
                buffer.push_str(grapheme);
                iter.next();
            }
            _ => {
                tokens.push(Token::Text(buffer));
                return;
            }
        }
    }
}

fn scan_identifier(iter: &mut Iter) -> String {
    let mut name = String::new();

    loop {
        match iter.peek() {
            Some((_index, "}")) | Some((_index, " ")) => {
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

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

#[derive(Debug)]
enum Node {
    Text(String),
    Reference(String),
}

fn parse(tokens: &mut TokenIter) -> Vec<Node> {
    let mut ast = vec![];

    loop {
        match tokens.peek() {
            Some(Token::Text(text)) => {
                ast.push(Node::Text(text.clone()));
                tokens.next();
            }
            Some(Token::Reference(name)) => {
                ast.push(Node::Reference(name.clone()));
                tokens.next();
            }
            Some(Token::OpenParen) | Some(Token::CloseParen) => {
                tokens.next();
            }
            None => {
                break;
            }
        }
    }

    ast
}

type NodeIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Node>>;

fn render(iter: &mut NodeIter) -> String {
    let mut builder_lines = String::new();
    let mut params = vec![];
    let mut has_builder = false;

    loop {
        match iter.peek() {
            Some(Node::Text(text)) => {
                iter.next();
                if has_builder {
                    builder_lines.push_str(&format!(
                        "    let builder = string_builder.append(builder, \"{}\")\n",
                        text
                    ));
                } else {
                    builder_lines.push_str(&format!(
                        "    let builder = string_builder.from_string(\"{}\")\n",
                        text
                    ));
                    has_builder = true;
                }
            }
            Some(Node::Reference(name)) => {
                iter.next();
                if has_builder {
                    builder_lines.push_str(&format!(
                        "    let builder = string_builder.append(builder, {})\n",
                        name
                    ));
                } else {
                    builder_lines.push_str(&format!(
                        "    let builder = string_builder.from_string({})\n",
                        name
                    ));
                    has_builder = true;
                }
                params.push(name.clone());
            }
            None => break,
        }
    }

    let output = format!(
        r#"import gleam/string_builder.{{StringBuilder}}

pub fn render_builder({}) -> StringBuilder {{
{}
    builder
}}

pub fn render({}) -> String {{
    string_builder.to_string(render_builder({}))
}}
"#,
        params.join(", "),
        builder_lines,
        params.join(", "),
        params.join(", "),
    );

    output
}

fn convert(filepath: &std::path::PathBuf) {
    let contents = std::fs::read_to_string(filepath).expect("Unable to read file");
    let tokens = scan(&contents);
    let ast = parse(&mut tokens.iter().peekable());
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
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format!("{:#?}", scan($text)),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_parse {
        ($text:expr $(,)?) => {{
            let tokens = scan($text);
            insta::assert_snapshot!(
                insta::internals::AutoName,
                format!("{:#?}", parse(&mut tokens.iter().peekable())),
                $text
            );
        }};
    }

    #[macro_export]
    macro_rules! assert_render {
        ($text:expr $(,)?) => {{
            let tokens = scan($text);
            let ast = parse(&mut tokens.iter().peekable());
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
        assert_scan!("Hello { name }, good to meet you");
    }

    #[test]
    fn test_scan_two_identifiers() {
        assert_scan!("Hello { name }, { adjective } to meet you");
    }

    #[test]
    fn test_scan_escaped_parens() {
        assert_scan!("Hello {{ name }}, good to meet you");
    }

    // Parse

    #[test]
    fn test_parse_pure_text() {
        assert_parse!("Hello name, good to meet you");
    }

    #[test]
    fn test_parse_escaped_parens() {
        assert_parse!("Hello {{ name }}, good to meet you");
    }

    #[test]
    fn test_parse_identifier() {
        assert_parse!("Hello { name }, good to meet you");
    }

    // Render

    #[test]
    fn test_render_pure_text() {
        assert_render!("Hello name, good to meet you");
    }

    #[test]
    fn test_render_identifier() {
        assert_render!("Hello { name }, good to meet you");
    }

    #[test]
    fn test_render_two_identifiers() {
        assert_render!("Hello { name }, { adjective } to meet you");
    }
}
