use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::{Value, Block, Intrinsic, SourceBlock, Result, Error, Intrinsics, Object, Span, Pos, Note};
use crate::lexer::{Token, TokenKind};
use crate::muncher::{MunchOutput, Muncher};

enum EvalBreak {
    Error(Error),
    Return(Value),
}

impl From<Error> for EvalBreak {
    fn from(err: Error) -> Self {
        EvalBreak::Error(err)
    }
}

type EvalResult<T> = std::result::Result<T, EvalBreak>;

fn double_def(token: Token, prev: Option<Span>) -> Error {
    Error {
        msg: format!("variable `{}` is already defined in this scope", token.source),
        span: token.span,
        notes: if let Some(span) = prev {
            Vec::from([
                Note {
                    msg: "previous definition here".to_owned(),
                    span,
                },
            ])
        } else {
            Vec::new()
        },
    }
}

fn undefined_var(token: Token) -> Error {
    Error {
        msg: format!("variable `{}` is not defined", token.source),
        span: token.span,
        notes: Vec::new(),
    }
}

#[derive(Debug)]
struct Def {
    value: Value,
    span: Option<Span>,
}

#[derive(Debug)]
struct EnvInner {
    values: RefCell<HashMap<Rc<str>, Def>>,
    next: Option<Rc<EnvInner>>,
}

impl EnvInner {
    fn define(&self, ident: Token, value: Value) -> Result<()> {
        let prev = self.values.borrow_mut().insert(
            ident.source.clone(),
            Def { value, span: Some(ident.span) },
        );
        if let Some(prev) = prev {
            Err(double_def(ident, prev.span))
        } else {
            Ok(())
        }
    }

    fn get_raw(&self, var: &str) -> Option<Value> {
        if let Some(def) = self.values.borrow().get(var) {
            Some(def.value.clone())
        } else {
            self.next.as_ref().and_then(|e| e.get_raw(var))
        }
    }

    fn set(&self, ident: Token, value: Value) -> Result<()> {
        let mut values = self.values.borrow_mut();
        if values.contains_key(&ident.source) {
            values.get_mut(&ident.source).unwrap().value = value;
            return Ok(());
        } else if let Some(env) = &self.next {
            env.set(ident, value)
        } else {
            Err(undefined_var(ident))
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Env {
    inner: Rc<EnvInner>,
    this: Option<Value>,
}

impl Env {
    pub(crate) fn new() -> Env {
        Env {
            inner: Rc::new(EnvInner {
                values: Default::default(),
                next: None,
            }),
            this: None,
        }
    }

    pub(crate) fn define(&self, ident: Token, value: Value) -> Result<()> {
        self.inner.define(ident, value)
    }

    pub(crate) fn get(&self, ident: Token) -> Result<Value> {
        if let Some(value) = self.inner.get_raw(&ident.source) {
            Ok(value)
        } else if let Ok(num) = ident.source.parse::<i64>() {
            Ok(Value::Int(num))
        } else if let Ok(b) = ident.source.parse::<bool>() {
            Ok(Value::Bool(b))
        } else if &*ident.source == "nil" {
            Ok(Value::Nil)
        } else {
            Err(undefined_var(ident))
        }
    }

    pub(crate) fn set(&self, ident: Token, value: Value) -> Result<()> {
        self.inner.set(ident, value)
    }

    pub(crate) fn scope(&self) -> Env {
        Env {
            inner: Rc::new(EnvInner {
                values: Default::default(),
                next: Some(self.inner.clone()),
            }),
            this: self.this.clone(),
        }
    }

    pub(crate) fn define_raw(&self, name: &str, value: Value) {
        self.inner.values.borrow_mut().insert(name.into(), Def { value, span: None });
    }
}

pub(crate) struct Interpreter {
    pub(crate) env: Env,
    pub(crate) intrinsics: Rc<dyn Intrinsics>,
}

impl Interpreter {
    pub(crate) fn block_with_closure(&mut self, block: &Block) -> Result<Value> {
        match block {
            Block::Source(s) => self.block_with_env(block, s.closure.clone()),
            Block::Intrinsic(_) => self.block_inner(block),
        }
    }

    pub(crate) fn block_with_env(&mut self, block: &Block, env: Env) -> Result<Value> {
        let mut block_interp = Interpreter {
            env,
            intrinsics: self.intrinsics.clone(),
        };
        block_interp.block_inner(&block)
    }

    fn block_inner(&mut self, block: &Block) -> Result<Value> {
        match block {
            Block::Source(s) => match self.source_block(s) {
                Ok(()) => Ok(Value::Nil),
                Err(EvalBreak::Error(err)) => Err(err),
                Err(EvalBreak::Return(ret)) => Ok(ret),
            },
            Block::Intrinsic(Intrinsic::Print(str)) => {
                self.intrinsics.print(&str);
                Ok(Value::Nil)
            }
            Block::Intrinsic(Intrinsic::Value(value)) => {
                Ok(value.clone())
            }
        }
    }

    fn source_block(&mut self, block: &SourceBlock) -> EvalResult<()> {
        let mut tokens = Tokens {
            tokens: block.tokens.clone(),
            idx: 0,
        };
        while tokens.peek().is_some() {
            self.stmt(&mut tokens)?;
        }
        Ok(())
    }

    fn stmt(&mut self, tokens: &mut Tokens) -> EvalResult<()> {
        if tokens.peek().is_none() {
            Ok(())
        } else if let Some(_let) = tokens.check(|t| t.kind == TokenKind::Let) {
            let ident = tokens.expect(Token::is_ident, "identifier")?;
            tokens.expect(Token::is_eq, "`=`")?;
            let value = self.expr(tokens)?.value;
            self.env.define(ident, value)?;
            Ok(())
        } else if let Some(_return) = tokens.check(|t| t.kind == TokenKind::Return) {
            let value = self.expr(tokens)?.value;
            Err(EvalBreak::Return(value))
        } else if let Some(_semi) = tokens.check(Token::is_semi) {
            Ok(())
        } else {
            self.expr(tokens)?;
            Ok(())
        }
    }

    pub(crate) fn expr(&mut self, tokens: &mut Tokens) -> Result<SpannedValue> {
        if let Some(ident) = tokens.check(Token::is_ident) {
            if let Some(_eq) = tokens.check(Token::is_eq) {
                let value = self.expr(tokens)?;
                self.env.set(ident, value.value.clone())?;
                Ok(value)
            } else {
                let span = ident.span;
                let value = self.env.get(ident)?;
                let spanned = SpannedValue {
                    value,
                    span,
                };
                self.munch_calls(spanned, tokens)
            }
        } else if let Some(this) = tokens.check(|t| t.kind == TokenKind::This) {
            let this = if let Some(value) = self.env.this.clone() {
                SpannedValue { value, span: this.span }
            } else {
                return Err(Error {
                    msg: "cannot use `self` outside methods".to_owned(),
                    span: this.span,
                    notes: Vec::new(),
                });
            };
            if let Some((_dot, ident)) = eat_token2(
                tokens,
                Token::is_dot,
                |t| t.is_ident() && this.value.has_field(&t.source),
            ) {
                if let Some(_eq) = tokens.check(Token::is_eq) {
                    let value = self.expr(tokens)?;
                    this.value.on_field(&ident.source, |f| *f = value.value.clone());
                    Ok(SpannedValue {
                        value: value.value,
                        span: this.span.union(value.span),
                    })
                } else {
                    let mut field = None;
                    this.value.on_field(&ident.source, |f| field = Some(f.clone()));
                    let span = this.span.union(ident.span);
                    let value = SpannedValue {
                        value: field.unwrap(),
                        span,
                    };
                    self.munch_calls(value, tokens)
                }
            } else {
                self.munch_calls(this, tokens)
            }
        } else if let Some(str) = tokens.check(|t| t.kind == TokenKind::String) {
            let value = Value::String(crate::lexer::unescape_string(&str.source).into());
            let spanned = SpannedValue {
                value,
                span: str.span,
            };
            self.munch_calls(spanned, tokens)
        } else if let Some(obj) = tokens.check(|t| t.kind == TokenKind::Object) {
            let name = tokens.check(Token::is_ident).map(|t| t.source);
            tokens.expect(Token::is_left_brace, "`{`")?;
            let (properties, matchers) = self.munch_object_contents(tokens)?;
            let muncher = compile_object_muncher(matchers)?;
            let object = Value::Object(Rc::new(Object {
                name,
                properties: RefCell::new(properties),
                muncher,
            }));
            let spanned = SpannedValue {
                value: object,
                span: obj.span.union(tokens.prev_span()),
            };
            self.munch_calls(spanned, tokens)
        } else if let Some(left) = tokens.check(Token::is_left_paren) {
            let value = self.expr(tokens)?.value;
            let right = tokens.expect(Token::is_right_paren, "`)`")?;
            let span = left.span.union(right.span);
            let spanned = SpannedValue { value, span };
            self.munch_calls(spanned, tokens)
        } else {
            return Err(tokens.error("expected expression"));
        }
    }

    fn munch_calls(&mut self, value: SpannedValue, tokens: &mut Tokens) -> Result<SpannedValue> {
        match tokens.peek() {
            Some(t) if !can_start_call(t) => return Ok(value),
            None => return Ok(value),
            _ => {}
        }
        let start = tokens.peek().unwrap().span;
        let mut muncher = value.value.get_muncher();
        let env = Env::new();
        let block = loop {
            match muncher.munch(tokens, &env, self) {
                MunchOutput::Done { block } => {
                    break block;
                }
                MunchOutput::Continue { muncher: next } => {
                    muncher = next;
                }
                MunchOutput::Failed { mut error } => {
                    error.notes.push(Note {
                        msg: format!("call to {} started here", value.value.type_name()),
                        span: start,
                    });
                    return Err(error);
                }
                MunchOutput::FailedEval { error } => return Err(error),
            }
        };
        let span = value.span.union(tokens.prev_span());
        let env = Env {
            inner: Rc::new(EnvInner {
                values: Rc::try_unwrap(env.inner).unwrap().values,
                next: match &*block {
                    Block::Source(s) => Some(s.closure.inner.clone()),
                    Block::Intrinsic(_) => None,
                },
            }),
            this: Some(value.value.clone()),
        };
        let returned_value = self.block_with_env(&block, env)?;
        let spanned = SpannedValue {
            value: returned_value,
            span,
        };
        self.munch_calls(spanned, tokens)
    }

    fn munch_object_contents(&mut self, tokens: &mut Tokens) -> Result<(HashMap<Rc<str>, Value>, Vec<Matcher>)> {
        let mut matchers = Vec::new();
        let mut properties = HashMap::new();
        let mut property_spans = HashMap::new();
        while tokens.check(|t| t.kind == TokenKind::This).is_some() {
            tokens.expect(Token::is_dot, "`.`")?;
            let name = tokens.expect(Token::is_ident, "identifier")?;
            tokens.expect(Token::is_eq, "`=`")?;
            let value = self.expr(tokens)?.value;
            tokens.expect(Token::is_semi, "`;`")?;
            if let Some(span) = property_spans.get(&*name.source).copied() {
                return Err(Error {
                    msg: format!("property {} defined twice", name.source),
                    span: name.span,
                    notes: Vec::from([
                        Note {
                            msg: "previously defined here".to_owned(),
                            span,
                        }
                    ])
                })
            }
            property_spans.insert(name.source.clone(), name.span);
            properties.insert(name.source, value);
        }
        while !tokens.at(Token::is_right_brace) {
            let mut pattern = Vec::new();
            if !can_start_call(&tokens.peek().unwrap()) {
                return Err(tokens.error(&format!("can't start call with {}", tokens.peek().unwrap())));
            }
            while !tokens.at(Token::is_left_brace) && !tokens.at(Token::is_right_brace) {
                if let Some(_dollar) = tokens.check(Token::is_dollar) {
                    let bind = tokens.expect(Token::is_ident, "identifier")?;
                    tokens.expect(Token::is_colon, "`:`")?;
                    let kind = tokens.expect(Token::is_ident, "identifier")?;
                    pattern.push(Pattern::Var { kind, bind });
                } else {
                    pattern.push(Pattern::Token(tokens.check(|_| true).unwrap()));
                }
            }
            if tokens.at(Token::is_right_brace) {
                return Err(tokens.error("missing method body"));
            }
            let left_brace = tokens.advance();
            let body = self.munch_source_block_inner(tokens);
            let body = Rc::new(Block::Source(body));
            matchers.push(Matcher { pattern, left_brace, body });
        }
        tokens.advance();
        Ok((properties, matchers))
    }

    fn munch_source_block_inner(&mut self, tokens: &mut Tokens) -> SourceBlock {
        let mut contents = Vec::new();
        let mut depth = 0;
        while depth != 0 || !tokens.peek().unwrap().is_right_brace() {
            if tokens.at(Token::is_left_brace) {
                depth += 1;
            } else if tokens.at(Token::is_right_brace) {
                depth -= 1;
            }
            contents.push(tokens.advance());
        }
        tokens.advance();
        SourceBlock {
            closure: self.env.clone(),
            tokens: contents.into(),
        }
    }

    pub(crate) fn munch_source_block(&mut self, tokens: &mut Tokens) -> Result<SourceBlock> {
        tokens.expect(Token::is_left_brace, "block")?;
        Ok(self.munch_source_block_inner(tokens))
    }
}

pub(crate) struct SpannedValue {
    pub(crate) value: Value,
    pub(crate) span: Span,
}

pub(crate) struct Tokens {
    tokens: Rc<[Token]>,
    idx: usize,
}

impl Tokens {
    pub(crate) fn check(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Token> {
        if self.at(pred) {
            Some(self.advance())
        } else {
            None
        }
    }

    pub(crate) fn advance(&mut self) -> Token {
        let token = self.tokens[self.idx].clone();
        self.idx += 1;
        token
    }

    pub(crate) fn at(&self, pred: impl FnOnce(&Token) -> bool) -> bool {
        match self.peek() {
            Some(tok) => pred(tok),
            _ => false,
        }
    }

    pub(crate) fn expect(&mut self, pred: impl FnOnce(&Token) -> bool, msg: &str) -> Result<Token> {
        if self.at(pred) {
            Ok(self.advance())
        } else {
            Err(self.error(&format!("expected {}", msg)))
        }
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.idx)
    }

    pub(crate) fn nth(&self, idx: usize) -> Option<&Token> {
        self.tokens.get(self.idx + idx)
    }

    fn prev_span(&self) -> Span {
        assert!(self.idx > 0);
        self.tokens[self.idx - 1].span
    }

    /// Create an error on next token.
    pub(crate) fn error(&self, msg: &str) -> Error {
        assert!(self.tokens.len() > 0);
        let span = self.tokens
            .get(self.idx)
            .map(|t| t.span)
            .or_else(|| self.tokens
                .get(self.idx - 1)
                .map(|t| Span {
                    start: t.span.end,
                    end: Pos {
                        line: t.span.end.line,
                        col: t.span.end.col + 1,
                        offset: t.span.end.offset + 1,
                    }
                }))
            .unwrap();
        Error {
            msg: msg.to_owned(),
            span,
            notes: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn debug(&self, msg: &str) {
        println!("{}: {:?}", msg, self.tokens[self.idx..].iter().map(|t| &*t.source).collect::<Vec<_>>());
    }
}

fn compile_object_muncher(matchers: Vec<Matcher>) -> Result<Rc<dyn Muncher>> {
    if matchers.len() == 0 {
        return Ok(Rc::new(crate::muncher::NoMuncher));
    }
    let trie = Rc::new(RefCell::new(crate::munch_trie::Node::default()));
    for matcher in matchers {
        let mut trie = trie.clone();
        let mut bindings = Vec::new();
        for pattern in matcher.pattern {
            match pattern {
                Pattern::Var { kind, bind } => {
                    let next_trie = match &*kind.source {
                        "expr" => trie.borrow_mut().step_expr(kind.span)?,
                        "ident" => trie.borrow_mut().step_ident(kind.span)?,
                        "block" => trie.borrow_mut().step_block(kind.span)?,
                        _ => return Err(Error {
                            msg: format!("invalid muncher type"),
                            span: kind.span,
                            notes: Vec::new(),
                        }),
                    };
                    trie = next_trie;
                    bindings.push(bind);
                }
                Pattern::Token(tok) => {
                    let next_trie = trie.borrow_mut().step_token(&tok)?;
                    trie = next_trie;
                }
            }
        }
        trie.borrow_mut().done(matcher.left_brace.span, bindings, matcher.body)?;
    }
    Ok(Rc::new(crate::munch_trie::TrieMuncher {
        trie,
        values: Vec::new(),
    }))
}

#[derive(Clone)]
enum Pattern {
    Var { kind: Token, bind: Token },
    Token(Token),
}

struct Matcher {
    pattern: Vec<Pattern>,
    left_brace: Token,
    body: Rc<Block>,
}

fn can_start_call(token: &Token) -> bool {
    token.is_dot() || token.is_left_paren() || &*token.source == "["
}

pub(crate) fn can_start_expr(token: &Token) -> bool {
    token.is_ident()
    || token.kind == TokenKind::This
    || token.kind == TokenKind::String
    || token.kind == TokenKind::Object
    || token.is_left_paren()
}

fn eat_token2(
    tokens: &mut Tokens,
    check1: impl FnOnce(&Token) -> bool,
    check2: impl FnOnce(&Token) -> bool,
) -> Option<(Token, Token)> {
    match tokens.nth(0) {
        Some(t) if !check1(t) => return None,
        None => return None,
        _ => {}
    }
    match tokens.nth(1) {
        Some(t) if !check2(t) => return None,
        None => return None,
        _ => {}
    }
    let t1 = tokens.advance();
    let t2 = tokens.advance();
    Some((t1, t2))
}
