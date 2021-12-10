use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::{Block, Error, Result, Span, Note, Value};
use crate::interpreter::{Tokens, Env, Interpreter, can_start_expr};
use crate::lexer::Token;
use crate::muncher::{Muncher, MunchOutput};

struct Step {
    next: Rc<RefCell<Node>>,
    first_span: Span,
}

struct Done {
    bindings: Vec<Token>,
    block: Rc<Block>,
    first_span: Span,
}

#[derive(Default)]
pub(crate) struct Node {
    tokens: HashMap<Rc<str>, Step>,
    expr: Option<Step>,
    ident: Option<Step>,
    block: Option<Step>,
    done: Option<Done>,
}

impl Node {
    pub(crate) fn step_token(&mut self, token: &Token) -> Result<Rc<RefCell<Node>>> {
        assert_ne!(&*token.source, "{"); // }
        if &*token.source == "(" { // )
            if let Some(step) = &self.expr {
                return Err(Error {
                    msg: format!("`(` conflicts with expr muncher"), // )
                    span: token.span,
                    notes: Vec::from([
                        Note {
                            msg: "expr branch is here".to_owned(),
                            span: step.first_span,
                        }
                    ]),
                });
            }
        }
        Ok(self.tokens.entry(token.source.clone())
            .or_insert_with(|| Step {
                next: Default::default(),
                first_span: token.span,
            })
            .next
            .clone())
    }

    pub(crate) fn step_expr(&mut self, span: Span) -> Result<Rc<RefCell<Node>>> {
        if let Some(step) = self.tokens.get("(") { // )
            return Err(Error {
                msg: format!("expr muncher conflicts with `(`"), // )
                span,
                notes: Vec::from([
                    Note {
                        msg: "`(` branch is here".to_owned(), // )
                        span: step.first_span,
                    }
                ]),
            });
        }
        if let Some(step) = &self.ident {
            return Err(Error {
                msg: format!("expr muncher conflicts with ident muncher"),
                span,
                notes: Vec::from([
                    Note {
                        msg: "ident branch is here".to_owned(),
                        span: step.first_span,
                    }
                ]),
            });
        }
        Ok(self.expr
            .get_or_insert_with(|| Step {
                next: Default::default(),
                first_span: span,
            })
            .next
            .clone())
    }

    pub(crate) fn step_ident(&mut self, span: Span) -> Result<Rc<RefCell<Node>>> {
        if let Some(step) = &self.expr {
            return Err(Error {
                msg: format!("ident muncher conflicts with expr muncher"),
                span,
                notes: Vec::from([
                    Note {
                        msg: "expr branch is here".to_owned(),
                        span: step.first_span,
                    }
                ]),
            });
        }
        Ok(self.ident
            .get_or_insert_with(|| Step {
                next: Default::default(),
                first_span: span,
            })
            .next
            .clone())
    }

    pub(crate) fn step_block(&mut self, span: Span) -> Result<Rc<RefCell<Node>>> {
        Ok(self.block
            .get_or_insert_with(|| Step {
                next: Default::default(),
                first_span: span,
            })
            .next
            .clone())
    }

    pub(crate) fn done(&mut self, span: Span, bindings: Vec<Token>, block: Rc<Block>) -> Result<()> {
        match &self.done {
            Some(done) => Err(Error {
                msg: format!("method already defined"),
                span,
                notes: Vec::from([
                    Note {
                        msg: "previous definition here".to_owned(),
                        span: done.first_span,
                    }
                ]),
            }),
            None => {
                self.done = Some(Done {
                    first_span: span,
                    bindings,
                    block,
                });
                Ok(())
            }
        }
    }
}

pub(crate) struct TrieMuncher {
    pub(crate) trie: Rc<RefCell<Node>>,
    pub(crate) values: Vec<Value>,
}

impl TrieMuncher {
    fn done(&self, env: &Env) -> std::result::Result<MunchOutput, MunchOutput> {
        let node = self.trie.borrow();
        let done = node.done.as_ref().unwrap();
        assert_eq!(done.bindings.len(), self.values.len());
        for (bind, value) in done.bindings.iter().cloned().zip(self.values.iter().cloned()) {
            env.define(bind, value)?;
        }
        Ok(MunchOutput::Done {
            block: done.block.clone(),
        })
    }

    fn format_expectations(&self) -> String {
        let mut expectations = Vec::new();
        let node = self.trie.borrow();
        for key in node.tokens.keys() {
            expectations.push(format!("`{}`", key));
        }
        if node.expr.is_some() {
            expectations.push("expression".to_owned());
        }
        if node.ident.is_some() {
            expectations.push("identifier".to_owned());
        }
        if node.block.is_some() {
            expectations.push("block".to_owned());
        }
        expectations.sort();
        match expectations.as_slice() {
            [] => panic!("no expectations"),
            [single] => single.clone(),
            [a, b] => format!("{} or {}", a, b),
            [a, b, c] => format!("{}, {}, or {}", a, b, c),
            [a, rest @ ..] => format!("{} or {} other options", a, rest.len()),
        }
    }
}

impl Muncher for TrieMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> std::result::Result<MunchOutput, MunchOutput> {
        let node = self.trie.borrow();
        match tokens.peek() {
            Some(token) => {
                if let Some(step) = node.tokens.get(&*token.source) {
                    tokens.advance();
                    Ok(MunchOutput::Continue {
                        muncher: Rc::new(TrieMuncher {
                            trie: step.next.clone(),
                            values: self.values.clone(),
                        })
                    })
                } else if node.expr.is_some() && tokens.at(can_start_expr) {
                    let value = caller.expr(tokens)?.value;
                    let mut values = self.values.clone();
                    values.push(value);
                    Ok(MunchOutput::Continue {
                        muncher: Rc::new(TrieMuncher {
                            trie: node.expr.as_ref().unwrap().next.clone(),
                            values,
                        })
                    })
                } else if node.ident.is_some() && tokens.at(Token::is_ident) {
                    let ident = tokens.advance();
                    let mut values = self.values.clone();
                    values.push(Value::Ident(ident.source));
                    Ok(MunchOutput::Continue {
                        muncher: Rc::new(TrieMuncher {
                            trie: node.ident.as_ref().unwrap().next.clone(),
                            values,
                        })
                    })
                } else if node.block.is_some() && tokens.at(Token::is_left_brace) {
                    let block = caller.munch_source_block(tokens)?;
                    let mut values = self.values.clone();
                    values.push(Value::Block(Rc::new(Block::Source(block))));
                    Ok(MunchOutput::Continue {
                        muncher: Rc::new(TrieMuncher {
                            trie: node.block.as_ref().unwrap().next.clone(),
                            values,
                        })
                    })
                } else if let Some(_) = &node.done {
                    self.done(env)
                } else {
                    Err(MunchOutput::Failed {
                        error: tokens.error(&format!("expected {}", self.format_expectations())),
                    })
                }
            }
            None => {
                if let Some(_) = &node.done {
                    self.done(env)
                } else {
                    Err(MunchOutput::Failed {
                        error: tokens.error(&format!("expected {}", self.format_expectations())),
                    })
                }
            }
        }
    }
}
