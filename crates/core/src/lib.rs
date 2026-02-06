#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Symbol,
    Whitespace,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    pub kind: String,
    pub children: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn parse(_input: &str) -> Result<AstNode, ParseError> {
    Err(ParseError {
        message: "parse not implemented".to_string(),
        diagnostics: Vec::new(),
    })
}
