use parser::Span;
use std::cell::RefCell;

#[must_use]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Report {
    pub level: Level,
    pub main_note: Note,
    pub extra_notes: Vec<Note>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Note {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Level {
    Error,
    // TODO(tsion): Introduce this once it's needed.
    // Warning,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Reporter {
    pub errors: Vec<Report>,
}

impl Reporter {
    fn new() -> Self {
        Reporter { errors: Vec::new() }
    }
}

thread_local!(pub static REPORTER: RefCell<Reporter> = RefCell::new(Reporter::new()));

impl Report {
    pub fn new<S, T>(level: Level, span: S, message: T, notes: Vec<Note>) -> Self
            where S: Into<Span>, T: Into<String> {
        Report {
            level: level,
            main_note: Note { span: span.into(), message: message.into() },
            extra_notes: notes,
        }
    }

    pub fn report(self) {
        REPORTER.with(|reporter| reporter.borrow_mut().errors.push(self));
    }
}

pub fn report<S, T>(level: Level, span: S, message: T) where S: Into<Span>, T: Into<String> {
    Report::new(level, span.into(), message.into(), Vec::new()).report()
}

pub fn report_with_notes<S, T>(level: Level, span: S, message: T, notes: Vec<Note>)
        where S: Into<Span>, T: Into<String> {
    Report::new(level, span.into(), message.into(), notes).report()
}

pub fn note<S, T>(span: S, message: T) -> Note where S: Into<Span>, T: Into<String> {
    Note { span: span.into(), message: message.into() }
}
