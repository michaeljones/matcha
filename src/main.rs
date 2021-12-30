use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

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

// fn parse(contents: &str) -> () {}

fn main() {
    let contents = r#"
Hi <% name %>,

Welcome to the project.

Kind regards,
The Team
"#;

    let tokens = scan(contents);
    println!("{:?}", tokens);
    // parse(contents);

    ()
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

    #[test]
    fn test_pure_text() {
        assert_scan!("Hello name, good to meet you");
    }

    #[test]
    fn test_identifier() {
        assert_scan!("Hello { name }, good to meet you");
    }

    #[test]
    fn test_two_identifiers() {
        assert_scan!("Hello { name }, { adjective } to meet you");
    }

    #[test]
    fn test_escaped_parens() {
        assert_scan!("Hello {{ name }}, good to meet you");
    }
}
