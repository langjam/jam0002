use std::fmt;
use std::rc::Rc;
use logos::Logos;
use crate::{Pos, Result, Span, Error, Note};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TokenKind {
    #[regex(r"[a-zA-Z0-9_]+")]
    Ident,
    #[regex(r#""([^\\"\n]|(\\["nrt\\]))*""#)]
    String,
    #[regex(r#"[^a-zA-Z0-9_" \t\r\n]"#)]
    Symbol,
    #[token("object")]
    Object,
    #[token("self")]
    This,
    #[token("return")]
    Return,
    #[token("let")]
    Let,
    #[regex(r"[ \t\r\n]+")]
    Whitespace,
    #[regex(r"//[^\n]*")]
    Comment,
    #[error]
    Error,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) span: Span,
    pub(crate) kind: TokenKind,
    pub(crate) source: Rc<str>,
}

impl Token {
    pub(crate) fn is_ident(&self) -> bool {
        self.kind == TokenKind::Ident
    }

    pub(crate) fn is_eq(&self) -> bool {
        &*self.source == "="
    }

    pub(crate) fn is_semi(&self) -> bool {
        &*self.source == ";"
    }

    pub(crate) fn is_dot(&self) -> bool {
        &*self.source == "."
    }

    pub(crate) fn is_left_paren(&self) -> bool {
        &*self.source == "(" // ) rainbow brackets!
    }

    pub(crate) fn is_right_paren(&self) -> bool {
        // ( rainbow brackets!
        &*self.source == ")"
    }

    pub(crate) fn is_left_brace(&self) -> bool {
        &*self.source == "{" // } rainbow brackets!
    }

    pub(crate) fn is_right_brace(&self) -> bool {
        // { rainbow brackets!
        &*self.source == "}"
    }

    pub(crate) fn is_dollar(&self) -> bool {
        &*self.source == "$"
    }

    pub(crate) fn is_colon(&self) -> bool {
        &*self.source == ":"
    }

    pub(crate) fn is_comma(&self) -> bool {
        &*self.source == ","
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Symbol => write!(f, "`{}`", self.source),
            TokenKind::Object => write!(f, "`object`"),
            TokenKind::This => write!(f, "`self`"),
            TokenKind::Return => write!(f, "`return`"),
            TokenKind::Let => write!(f, "`let`"),
            TokenKind::Whitespace => write!(f, "whitespace"),
            TokenKind::Comment => write!(f, "comment"),
            TokenKind::Error => write!(f, "bad token"),
        }
    }
}

fn lex(source: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut pos = Pos { line: 1, col: 1, offset: 0 };
    for (kind, span) in TokenKind::lexer(source).spanned() {
        let source = &source[span.clone()];
        let start = pos;
        for c in source.chars() {
            pos.offset += c.len_utf8();
            if c == '\n' {
                pos.line += 1;
                pos.col = 1;
            } else {
                pos.col += 1;
            }
        }
        let span = Span { start, end: pos };
        match kind {
            TokenKind::Whitespace | TokenKind::Comment => {}
            TokenKind::Error => return Err(Error {
                msg: "bad token".into(),
                span,
                notes: Vec::new(),
            }),
            _ => {
                tokens.push(Token {
                    span,
                    kind,
                    source: source.into(),
                });
            }
        }
    }
    Ok(tokens)
}

fn check_balance(tokens: &[Token]) -> Result<()> {
    let mut stack = Vec::new();
    for tok in tokens {
        if tok.is_left_brace() {
            stack.push((tok, "}"));
        } else if tok.is_left_paren() {
            stack.push((tok, ")"));
        } else if tok.is_right_brace() || tok.is_right_paren() {
            match stack.pop() {
                None => return Err(Error {
                    msg: "unopened delimiter".to_owned(),
                    span: tok.span,
                    notes: Vec::new(),
                }),
                Some((_, s)) if s == &*tok.source => {}
                Some((open, _)) => return Err(Error {
                    msg: "invalid delimiter".to_owned(),
                    span: tok.span,
                    notes: Vec::from([
                        Note {
                            msg: "currently open delimiter".to_owned(),
                            span: open.span,
                        }
                    ]),
                }),
            }
        }
    }
    Ok(())
}

pub(crate) fn lex_program(source: &str) -> Result<Vec<Token>> {
    let tokens = lex(source)?;
    check_balance(&tokens)?;
    Ok(tokens)
}

pub(crate) fn unescape_string(str: &str) -> Vec<char> {
    // strip quotes
    let str = &str[1..(str.len() - 1)];
    let mut res = Vec::new();
    let mut chars = str.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            res.push(match chars.next().unwrap() {
                '\\' => '\\',
                '"' => '"',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => unreachable!(),
            });
        } else {
            res.push(c);
        }
    }
    res
}
