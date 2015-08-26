use std::io::prelude::*;

use std::{io, mem};
use std::fs::File;
use std::path::Path;

use ast;

// Lexer tokens

#[derive(Clone, Debug, Eq, PartialEq)]
enum Token {
    Ident(String),
    StrLit(String),

    KeywordFn,

    ParenL,
    ParenR,
    BraceL,
    BraceR,
    Semicolon,

    Eof,
    Invalid,
}

// #[derive(Clone, Debug)]
// struct TokenAndSpan {
//     token: Token,
//     span: Span,
// }

// /// A reference to a contiguous region of a source file.
// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
// struct Span {
//     start: usize,
//     end: usize,
// }


// Errors

#[derive(Clone, Debug, Eq, PartialEq)]
struct Error {
    message: String,
    // span: Span,
}


// Lexer

#[derive(Clone, Debug)]
struct Lexer<'src> {
    source: &'src str,
    position: usize,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source: source,
            position: 0,
        }
    }

    fn next_token(&mut self) -> Token {
        let c = match self.read_char() {
            Some(c) => c,
            None    => return Token::Eof,
        };

        match c {
            c if is_whitespace(c) => {
                self.skip_whitespace();
                self.next_token()
            }

            c if is_ident_start(c) => {
                self.unread_char();
                self.lex_ident_or_keyword()
            }

            '"' => self.lex_string(),

            '(' => Token::ParenL,
            ')' => Token::ParenR,
            '{' => Token::BraceL,
            '}' => Token::BraceR,
            ';' => Token::Semicolon,

            _ => Token::Invalid,
        }
    }

    fn lex_ident_or_keyword(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(c) = self.read_char() {
            if !is_ident_continue(c) {
                self.unread_char();
                break;
            }

            ident.push(c);
        }

        match &ident[..] {
            "fn" => Token::KeywordFn,
            _ => Token::Ident(ident),
        }
    }

    fn lex_string(&mut self) -> Token {
        let mut string = String::new();

        while let Some(c) = self.read_char() {
            match c {
                '"' => break,
                '\\' => unimplemented!(),
                _ => string.push(c),
            }
        }

        Token::StrLit(string)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.read_char() {
            if !is_whitespace(c) {
                self.unread_char();
                break;
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        // Get the char that begins at self.position.
        let opt_c = self.source[self.position..].chars().next();

        if let Some(c) = opt_c {
            self.position += c.len_utf8();
        }

        opt_c
    }

    /// Step backwards one `char` in the input. Must not be called more times than `read_char` has
    /// been called.
    fn unread_char(&mut self) {
        assert!(self.position != 0);

        // Get the index of the char that comes before the char at self.position.
        let (prev_pos, _) = self.source[..self.position].char_indices().next_back().unwrap();
        self.position = prev_pos;
    }
}

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



// Parser

#[derive(Clone, Debug)]
struct Parser<'src> {
    lexer: Lexer<'src>,
    token: Token,
    last_token: Token,
    errors: Vec<Error>,
}

// Using a Result<T, ()> instead of Option<T> for the try! macro.
type ParseResult<T> = Result<T, ()>;

impl<'src> Parser<'src> {
    fn new(source: &'src str) -> Parser<'src> {
        let mut parser = Parser {
            lexer: Lexer::new(source),
            token: Token::Invalid,
            last_token: Token::Invalid,
            errors: Vec::new(),
        };

        parser.advance();

        parser
    }

    // fn parse_item(&mut self) -> ParseResult<ast::FnDef> {
    //     match self.token {
    //         Token::KeywordFn => self.parse_fn_def(),
    //         _ => Err(()),
    //     }
    // }

    fn parse_fn_def(&mut self) -> ParseResult<ast::FnDef> {
        try!(self.expect(&Token::KeywordFn));

        let name = match self.advance_and_get() {
            Token::Ident(name) => name,

            // TODO(tsion): Add an error to self.errors.
            _ => return Err(()),
        };

        try!(self.expect(&Token::ParenL));

        // TODO(tsion): Parse arguments.

        try!(self.expect(&Token::ParenR));
        try!(self.expect(&Token::BraceL));

        let body = try!(self.parse_expr());

        try!(self.expect(&Token::BraceR));

        Ok(ast::FnDef {
            name: name,
            args: Vec::new(),
            body: body,
        })
    }

    fn parse_expr(&mut self) -> ParseResult<ast::Expr> {
        let func = match self.advance_and_get() {
            Token::Ident(name) => name,

            // TODO(tsion): Add an error to self.errors.
            _ => return Err(()),
        };

        try!(self.expect(&Token::ParenL));

        let str = match self.advance_and_get() {
            Token::StrLit(str) => str,

            // TODO(tsion): Add an error to self.errors.
            _ => return Err(()),
        };

        try!(self.expect(&Token::ParenR));
        try!(self.expect(&Token::Semicolon));

        Ok(ast::Expr::FnCall {
            func: func,
            args: vec![ast::Expr::StrLit(str)],
        })
    }

    fn expect(&mut self, expected: &Token) -> ParseResult<()> {
        if self.token == *expected {
            self.advance();
            Ok(())
        } else {
            // TODO(tsion): Add an error to self.errors.
            println!("Expected '{:?}', found '{:?}'.", expected, self.token);
            Err(())
        }
    }

    fn advance(&mut self) {
        mem::swap(&mut self.last_token, &mut self.token);
        self.token = self.lexer.next_token();
    }

    fn advance_and_get(&mut self) -> Token {
        let old_token = mem::replace(&mut self.token, Token::Invalid);
        self.advance();
        old_token
    }
}

pub fn load_and_parse<P: AsRef<Path>>(path: P) -> io::Result<Option<ast::FnDef>> {
    let mut contents = String::new();
    let mut file = try!(File::open(path));
    try!(file.read_to_string(&mut contents));

    let mut parser = Parser::new(&contents);
    Ok(parser.parse_fn_def().ok())
}
