use ast;
use intern::Symbol;

use std::cell::RefCell;
use std::rc::Rc;

////////////////////////////////////////////////////////////////////////////////
// Lexer
////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(start: usize, end: usize, kind: TokenKind) -> Self {
        Token { kind: kind, span: Span::new(start, end) }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Ident(Symbol),
    StrLiteral(String),
    ParenL,
    ParenR,
    BraceL,
    BraceR,
    BracketL,
    BracketR,
    Semicolon,
    Invalid,
}

/// A reference to a contiguous region of a source file.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start, end: end }
    }

    pub fn point(pos: usize) -> Self {
        Span { start: pos, end: pos }
    }
}

#[derive(Clone, Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    position: usize,
    token_start: usize,
    current: Option<char>,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        use self::TokenKind::*;

        self.skip_whitespace();
        self.token_start = self.position;

        let c = match self.current {
            Some(c) => c,
            None => return None,
        };

        let kind = if is_ident_start(c) {
            Ident(self.lex_ident())
        } else {
            self.advance();
            match c {
                '"' => StrLiteral(self.lex_string()),
                '(' => ParenL,
                ')' => ParenR,
                '{' => BraceL,
                '}' => BraceR,
                '[' => BracketL,
                ']' => BracketR,
                ';' => Semicolon,
                _ => {
                    errors().report(Span::new(self.token_start, self.position),
                                    ErrorLevel::Error,
                                    "invalid source character");
                    Invalid
                }
            }
        };

        Some(Token::new(self.token_start, self.position, kind))
    }
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut lexer = Lexer { source: source, position: 0, token_start: 0, current: None };
        lexer.current = lexer.char_at(0);
        lexer
    }

    /// Lex an identifier (including keywords).
    fn lex_ident(&mut self) -> Symbol {
        while let Some(c) = self.current {
            if !is_ident_continue(c) { break; }
            self.advance();
        }

        Symbol::intern(&self.source[self.token_start..self.position])
    }

    /// Lex a string, assuming the initial " has already been consumed.
    fn lex_string(&mut self) -> String {
        let mut string = String::new();

        loop {
            let c = match self.current {
                Some(c) => c,
                None => {
                    errors().report(Span::point(self.position),
                                    ErrorLevel::Error,
                                    "unterminated string literal");
                    errors().report(Span::point(self.token_start),
                                    ErrorLevel::Note,
                                    "string literal began here");
                    break;
                }
            };

            let escape_start = self.position;
            self.advance();

            match c {
                '\\' => {
                    let escaped = match self.current { Some(c) => c, None => continue };
                    self.advance();

                    string.push(match escaped {
                        '\\' | '"' => escaped,
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        c => {
                            errors().report(Span::new(escape_start, self.position),
                                            ErrorLevel::Error,
                                            format!("invalid character escape: {:?}", c));
                            c
                        }
                    });
                }

                '"' => break,
                c => string.push(c),
            }
        }

        string
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            if !is_whitespace(c) { break; }
            self.advance();
        }
    }

    // fn report_error<S: Into<String>>(&self, message: S) {
    //     errors().report(Span::new(self.token_start, self.position), message.into());
    // }

    // fn peek(&self) -> Option<char> {
    //     self.current.and_then(|c| self.char_at(self.position + c.len_utf8()))
    // }

    fn char_at(&self, pos: usize) -> Option<char> {
        self.source[pos..].chars().next()
    }

    /// Move to the next char in the source code.
    fn advance(&mut self) {
        if let Some(c) = self.current {
            self.position += c.len_utf8();
            self.current = self.char_at(self.position);
        } else {
            panic!("lexer attempted to advance past the end of the source code");
        }
    }
}

pub fn tokenize(source: &str) -> Vec<Token> {
    Lexer::new(source).collect()
}

////////////////////////////////////////////////////////////////////////////////
// Parser
////////////////////////////////////////////////////////////////////////////////

pub fn parse_fn_def(tokens: &[Token]) -> Result<ast::FnDef, ()> {
    use self::TokenKind::*;

    if tokens[0].kind != Ident(Symbol::intern("fn")) {
        return Err(());
    }

    let name = match tokens[1].kind {
        Ident(name) => name,
        _ => {
            errors().report(tokens[1].span,
                            ErrorLevel::Error,
                            "expected identifier for fn name");
            return Err(());
        }
    };

    if tokens[2].kind != ParenL { return Err(()); }

    // TODO(tsion): Parse arguments.

    if tokens[3].kind != ParenR { return Err(()); }
    if tokens[4].kind != BraceL { return Err(()); }

    let mut body = Vec::new();
    let mut pos = 5;

    while tokens[pos].kind != BraceR {
        // body.push(try!(self.parse_expr()));
        pos += 1;
    }

    // Skip the closing brace.
    pos += 1;

    Ok(ast::FnDef {
        name: name,
        return_ty: ast::Type::Unit,
        args: Vec::new(),
        body: body,
    })
}

    // #[derive(Clone, Debug)]
    // struct Parser {
    // }

    // fn parse_module(&mut self) -> ParseResult<ast::Module> {
    //     let mut fns = Vec::new();
    //     while self.token != Token::Eof {
    //         fns.push(try!(self.parse_fn_def()));
    //     }
    //     Ok(ast::Module { fns: fns })
    // }

    // fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
    //     let func = match self.advance_and_get() {
    //         Token::Ident(name) => name,

    //         // TODO(tsion): Add an error to self.errors.
    //         _ => return Err(()),
    //     };

    //     try!(self.expect(&Token::ParenL));

    //     let str = match self.advance_and_get() {
    //         Token::StrLiteral(str) => str,

    //         // TODO(tsion): Add an error to self.errors.
    //         _ => return Err(()),
    //     };

    //     try!(self.expect(&Token::ParenR));
    //     try!(self.expect(&Token::Semicolon));

    //     Ok(ast::Expr::FnCall {
    //         func: func,
    //         args: vec![ast::Expr::StrLiteral(str)],
    //     })
    // }

////////////////////////////////////////////////////////////////////////////////
// Character classes
////////////////////////////////////////////////////////////////////////////////

/// Returns `true` if the given character is whitespace.
fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

/// Returns `true` if the given character is a digit.
fn is_digit(c: char) -> bool {
    match c {
        '0'...'9' => true,
        _ => false,
    }
}

/// Returns `true` if the given character is valid at the start of an identifier.
fn is_ident_start(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' | '_' => true,
        _ => false,
    }
}

/// Returns `true` if the given character is valid after the start of an identifier.
fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || is_digit(c)
}

////////////////////////////////////////////////////////////////////////////////
// Errors
////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    pub message: String,
    pub level: ErrorLevel,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ErrorLevel {
    Error,
    Warning,
    Note,
}

pub struct ErrorReporter {
    pub errors: RefCell<Vec<Error>>,
}

impl ErrorReporter {
    fn new() -> Self {
        ErrorReporter { errors: RefCell::new(Vec::new()) }
    }

    fn report<S: Into<String>>(&self, span: Span, level: ErrorLevel, message: S) {
        self.errors.borrow_mut().push(Error {
            message: message.into(),
            level: level,
            span: span,
        });
    }

    fn is_empty(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    fn reset(&self) {
        self.errors.borrow_mut().clear();
    }
}

// TODO(tsion): Get rid of this Rc.
pub fn errors() -> Rc<ErrorReporter> {
    thread_local!(static KEY: Rc<ErrorReporter> = Rc::new(ErrorReporter::new()));
    KEY.with(|k| k.clone())
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use intern::Symbol;
    use super::{errors, ErrorLevel, Lexer, Span, Token, TokenKind};
    use super::ErrorLevel::*;
    use super::TokenKind::*;

    #[test]
    fn lex_basics() {
        lexer_test("(", vec![ParenL], &[]);
        lexer_test(")", vec![ParenR], &[]);
        lexer_test("[", vec![BracketL], &[]);
        lexer_test("]", vec![BracketR], &[]);
        lexer_test("{", vec![BraceL], &[]);
        lexer_test("}", vec![BraceR], &[]);
        lexer_test(";", vec![Semicolon], &[]);
    }

    #[test]
    fn lex_invalid() {
        lexer_test("@", vec![Invalid], &[
            (0, 1, Error, "invalid source character"),
        ]);
    }

    #[test]
    fn lex_empty_string() {
        lexer_test(r#""""#, vec![StrLiteral(String::from(""))], &[]);
    }

    #[test]
    fn lex_unterminated_empty_string() {
        lexer_test(r#"""#, vec![StrLiteral(String::from(""))], &[
            (1, 1, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_unterminated_escape_empty_string() {
        lexer_test(r#""\"#, vec![StrLiteral(String::from(""))], &[
            (2, 2, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_unterminated_escape_string() {
        lexer_test(r#""foo\"#, vec![StrLiteral(String::from("foo"))], &[
            (5, 5, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_string() {
        lexer_test(r#""foobar""#, vec![StrLiteral(String::from("foobar"))], &[]);
    }

    #[test]
    fn lex_unterminated_string() {
        lexer_test(r#""foobar"#, vec![StrLiteral(String::from("foobar"))], &[
            (7, 7, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_string_with_just_newline() {
        lexer_test(r#""\n""#, vec![StrLiteral(String::from("\n"))], &[]);
    }

    #[test]
    fn lex_unterminated_string_with_just_newline() {
        lexer_test(r#""\n"#, vec![StrLiteral(String::from("\n"))], &[
            (3, 3, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_string_with_newline() {
        lexer_test(r#""foo\nbar""#, vec![StrLiteral(String::from("foo\nbar"))], &[]);
    }

    #[test]
    fn lex_unterminated_string_with_newline() {
        lexer_test(r#""foo\nbar"#, vec![StrLiteral(String::from("foo\nbar"))], &[
            (9, 9, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_invalid_escape_string() {
        lexer_test(r#""foo\cbar""#, vec![StrLiteral(String::from("foocbar"))], &[
            (4, 6, Error, "invalid character escape: 'c'"),
        ]);
    }

    #[test]
    fn lex_unterminated_invalid_escape_string() {
        lexer_test(r#""foo\cbar"#, vec![StrLiteral(String::from("foocbar"))], &[
            (4, 6, Error, "invalid character escape: 'c'"),
            (9, 9, Error, "unterminated string literal"),
            (0, 0, Note, "string literal began here"),
        ]);
    }

    #[test]
    fn lex_hello_world() {
        lexer_test(
            include_str!("../examples/hello_world.rig"),
            vec![sym("fn"), sym("main"), ParenL, ParenR, BraceL, sym("print"), ParenL,
                 StrLiteral(String::from("Hello, world!")), ParenR, Semicolon, BraceR],
            &[]
        );
    }

    fn sym(s: &str) -> TokenKind {
        Ident(Symbol::intern(s))
    }

    fn lexer_test(source: &str,
                  expected: Vec<TokenKind>,
                  expected_errors: &[(usize, usize, ErrorLevel, &str)]) {
        let mut lexer = Lexer::new(source);

        for expected_token in expected {
            assert_eq!(lexer.next().unwrap().kind, expected_token);
        }

        let extra_tokens: Vec<Token> = lexer.collect();
        assert_eq!(extra_tokens, &[]);

        assert_errors(expected_errors);
    }

    fn assert_errors(expected_errors: &[(usize, usize, ErrorLevel, &str)]) {
        let reporter = errors();

        {
            let errors = reporter.errors.borrow();

            for (i, &(start, end, level, message)) in expected_errors.iter().enumerate() {
                assert_eq!(errors[i], super::Error {
                    message: String::from(message),
                    level: level,
                    span: Span::new(start, end),
                });
            }

            // Checking that there are no unexpected errors this way will pretty-print the
            // unexpected errors if they exist.
            assert_eq!(&errors[expected_errors.len()..], &[]);
        }

        reporter.reset();
    }
}
