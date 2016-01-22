use ast;
use error;
use error::Level::*;
use intern::Symbol;

use std::ops::Range;

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
    StringLit(Symbol),
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
    fn new(start: usize, end: usize) -> Span {
        Span { start: start, end: end }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span::new(range.start, range.end)
    }
}

impl From<usize> for Span {
    fn from(pos: usize) -> Self {
        Span::new(pos, pos)
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
                '"' => StringLit(self.lex_string()),
                '(' => ParenL,
                ')' => ParenR,
                '{' => BraceL,
                '}' => BraceR,
                '[' => BracketL,
                ']' => BracketR,
                ';' => Semicolon,
                _ => {
                    error::report(Error, self.token_start..self.position,
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
    fn lex_string(&mut self) -> Symbol {
        let mut string = String::new();

        loop {
            let c = match self.current {
                Some(c) => c,
                None => {
                    error::report_with_notes(Error, self.position,
                                             "unterminated string literal", vec![
                        error::note(self.token_start, "string literal began here"),
                    ]);
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
                            error::report(Error, escape_start..self.position,
                                          format!("invalid character escape: {:?}", c));
                            c
                        }
                    });
                }

                '"' => break,
                c => string.push(c),
            }
        }

        Symbol::intern(&string)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            if !is_whitespace(c) { break; }
            self.advance();
        }
    }

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
            error::report(Error, tokens[1].span, "expected identifier for fn name");
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
    //         Token::StringLit(str) => str,

    //         // TODO(tsion): Add an error to self.errors.
    //         _ => return Err(()),
    //     };

    //     try!(self.expect(&Token::ParenR));
    //     try!(self.expect(&Token::Semicolon));

    //     Ok(ast::Expr::FnCall {
    //         func: func,
    //         args: vec![ast::Expr::StringLit(str)],
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
// Tests
////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use error::{self, Level, Report, Note, note};
    use error::Level::*;
    use intern::Symbol;
    use super::{Lexer, Span, Token, TokenKind};
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
            err(Error, 0..1, "invalid source character", &[])
        ]);
    }

    #[test]
    fn lex_empty_string() {
        lexer_test(r#""""#, vec![str("")], &[]);
    }

    #[test]
    fn lex_unterminated_empty_string() {
        lexer_test(r#"""#, vec![str("")], &[
            err(Error, 1, "unterminated string literal", &[note(0, "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_unterminated_escape_empty_string() {
        lexer_test(r#""\"#, vec![str("")], &[
            err(Error, 2, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_unterminated_escape_string() {
        lexer_test(r#""foo\"#, vec![str("foo")], &[
            err(Error, 5, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_string() {
        lexer_test(r#""foobar""#, vec![str("foobar")], &[]);
    }

    #[test]
    fn lex_unterminated_string() {
        lexer_test(r#""foobar"#, vec![str("foobar")], &[
            err(Error, 7, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_string_with_just_newline() {
        lexer_test(r#""\n""#, vec![str("\n")], &[]);
    }

    #[test]
    fn lex_unterminated_string_with_just_newline() {
        lexer_test(r#""\n"#, vec![str("\n")], &[
            err(Error, 3, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_string_with_newline() {
        lexer_test(r#""foo\nbar""#, vec![str("foo\nbar")], &[]);
    }

    #[test]
    fn lex_unterminated_string_with_newline() {
        lexer_test(r#""foo\nbar"#, vec![str("foo\nbar")], &[
            err(Error, 9, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_invalid_escape_string() {
        lexer_test(r#""foo\cbar""#, vec![str("foocbar")], &[
            err(Error, 4..6, "invalid character escape: 'c'", &[]),
        ]);
    }

    #[test]
    fn lex_unterminated_invalid_escape_string() {
        lexer_test(r#""foo\cbar"#, vec![str("foocbar")], &[
            err(Error, 4..6, "invalid character escape: 'c'", &[]),
            err(Error, 9, "unterminated string literal", &[note(0 , "string literal began here")]),
        ]);
    }

    #[test]
    fn lex_hello_world() {
        lexer_test(
            include_str!("../examples/hello_world.rig"),
            vec![sym("fn"), sym("main"), ParenL, ParenR, BraceL, sym("print"), ParenL,
                 str("Hello, world!"), ParenR, Semicolon, BraceR],
            &[]
        );
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Helper functions
    ////////////////////////////////////////////////////////////////////////////////

    fn sym(s: &str) -> TokenKind {
        Ident(Symbol::intern(s))
    }

    fn str(s: &str) -> TokenKind {
        StringLit(Symbol::intern(s))
    }

    fn err<S, T>(level: error::Level, span: S, message: T, notes: &[error::Note]) -> error::Report
            where S: Into<Span>, T: Into<String> {
        error::Report::new(level, span.into(), message.into(), Vec::from(notes))
    }

    fn lexer_test(source: &str, expected: Vec<TokenKind>, expected_errors: &[error::Report]) {
        let mut lexer = Lexer::new(source);

        for expected_token in expected {
            assert_eq!(lexer.next().unwrap().kind, expected_token);
        }

        let extra_tokens: Vec<Token> = lexer.collect();
        assert_eq!(extra_tokens, &[]);

        error::REPORTER.with(|reporter| {
            let mut r = reporter.borrow_mut();

            for expected_error in expected_errors {
                match r.errors.iter().position(|e| e == expected_error) {
                    Some(i) => { let _ = r.errors.remove(i); },
                    None => panic!("expected error not found: {:?}", expected_error),
                }
            }

            if !r.errors.is_empty() {
                panic!("unexpected errors: {:?}", r.errors);
            }
        });
    }
}
