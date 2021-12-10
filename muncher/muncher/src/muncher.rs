use std::rc::Rc;
use crate::{Block, Error, Intrinsic, Value, Span, SourceBlock};
use crate::interpreter::{Env, Interpreter, Tokens, SpannedValue};
use crate::lexer::{Token, TokenKind};

pub(crate) enum MunchOutput {
    Done {
        block: Rc<Block>,
    },
    Continue {
        muncher: Rc<dyn Muncher>,
    },
    Failed { error: Error },
    FailedEval { error: Error },
}

impl MunchOutput {
    fn failed(error: Error) -> MunchOutput {
        MunchOutput::Failed { error }
    }
}

impl From<Error> for MunchOutput {
    fn from(error: Error) -> Self {
        Self::FailedEval { error }
    }
}

pub(crate) trait Muncher {
    fn munch(&self, tokens: &mut Tokens, env: &Env, caller: &mut Interpreter) -> MunchOutput {
        self.munch_inner(tokens, env, caller).unwrap_or_else(std::convert::identity)
    }

    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput>;
}

pub(crate) struct NoMuncher;

impl Muncher for NoMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        _caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        Ok(MunchOutput::Failed {
            error: tokens.error("no methods are defined"),
        })
    }
}

pub(crate) struct PrintMuncher;

impl Muncher for PrintMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_left_paren, "`(`").map_err(MunchOutput::failed)?;
        let value = caller.expr(tokens)?.value;
        tokens.expect(Token::is_right_paren, "`)`").map_err(MunchOutput::failed)?;
        Ok(MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Print(value.to_string()))),
        })
    }
}

pub(crate) struct NumMuncher {
    pub(crate) value: i64,
}

impl Muncher for NumMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_dot, "`.`").map_err(MunchOutput::failed)?;

        fn cast_int(value: Value, span: Span) -> Result<i64, Error> {
            if let Value::Int(i) = value {
                Ok(i)
            } else {
                Err(Error {
                    msg: format!("expected int, got {}", value.type_name()),
                    span,
                    notes: Vec::new(),
                })
            }
        }

        let handler: fn(_, _, i64) -> _ = match tokens
            .peek()
            .filter(|t| t.kind == TokenKind::Ident)
            .map(|t| &*t.source)
        {
            Some("add") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_add(i).into()),
            Some("sub") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_sub(i).into()),
            Some("mul") => |v, s, n| cast_int(v, s).map(|i| n.wrapping_mul(i).into()),
            Some("div") => |v, s, n| cast_int(v, s).and_then(|i| if i == 0 {
                Err(Error {
                    msg: "divisor is zero".to_owned(),
                    span: s,
                    notes: Vec::new(),
                })
            } else {
                Ok(n.wrapping_div(i).into())
            }),
            Some("mod") => |v, s, n| cast_int(v, s).and_then(|i| if i == 0 {
                Err(Error {
                    msg: "divisor is zero".to_owned(),
                    span: s,
                    notes: Vec::new(),
                })
            } else {
                Ok(n.wrapping_rem(i).into())
            }),
            Some("lt") => |v, s, n| cast_int(v, s).map(|i| (n < i).into()),
            Some("le") => |v, s, n| cast_int(v, s).map(|i| (n <= i).into()),
            Some("eq") => |v, s, n| cast_int(v, s).map(|i| (n == i).into()),
            Some("ne") => |v, s, n| cast_int(v, s).map(|i| (n != i).into()),
            Some("gt") => |v, s, n| cast_int(v, s).map(|i| (n > i).into()),
            Some("ge") => |v, s, n| cast_int(v, s).map(|i| (n >= i).into()),
            _ => return Err(MunchOutput::Failed {
                error: tokens.error("expected numeric method"),
            }),
        };
        tokens.advance();
        tokens.expect(Token::is_left_paren, "`(`").map_err(MunchOutput::failed)?;
        let value = match caller.expr(tokens) {
            Ok(x) => x,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        tokens.expect(Token::is_right_paren, "`)`").map_err(MunchOutput::failed)?;
        let result = match handler(value.value, value.span, self.value) {
            Ok(x) => x,
            Err(e) => return Err(MunchOutput::FailedEval { error: e }),
        };

        Ok(MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Value(result))),
        })
    }
}

pub(crate) struct BoolMuncher {
    pub(crate) value: bool,
}

impl Muncher for BoolMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_dot, "`.`").map_err(MunchOutput::failed)?;
        tokens.expect(|t| &*t.source == "pick", "`pick`").map_err(MunchOutput::failed)?;
        tokens.expect(Token::is_left_paren, "`(`").map_err(MunchOutput::failed)?;
        let value1 = match caller.expr(tokens) {
            Ok(x) => x.value,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        tokens.expect(Token::is_comma, "`,`").map_err(MunchOutput::failed)?;
        let value2 = match caller.expr(tokens) {
            Ok(x) => x.value,
            Err(error) => return Err(MunchOutput::FailedEval { error }),
        };
        tokens.expect(Token::is_right_paren, "`)`").map_err(MunchOutput::failed)?;

        let result = if self.value { value1 } else { value2 };

        Ok(MunchOutput::Done {
            block: Rc::new(Block::Intrinsic(Intrinsic::Value(result))),
        })
    }
}

pub(crate) struct BlockMuncher {
    pub(crate) value: Rc<Block>,
}

impl Muncher for BlockMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_dot, "`.`").map_err(MunchOutput::failed)?;
        match tokens
            .peek()
            .filter(|t| t.kind == TokenKind::Ident)
            .map(|t| &*t.source)
        {
            Some("exec") => {
                tokens.advance();
                let result = match caller.block_with_closure(&self.value) {
                    Ok(value) => value,
                    Err(error) => return Err(MunchOutput::FailedEval { error }),
                };
                Ok(MunchOutput::Done {
                    block: Rc::new(Block::Intrinsic(Intrinsic::Value(result))),
                })
            }
            Some("def") => {
                tokens.advance();
                tokens.expect(Token::is_left_paren, "`(`")?;
                let ident = match caller.expr(tokens)? {
                    SpannedValue { value: Value::Ident(i), .. } => i,
                    SpannedValue { span, value } => return Err(MunchOutput::FailedEval {
                        error: Error {
                            msg: format!("expected <Ident>, got {}", value.type_name()),
                            span,
                            notes: Vec::new(),
                        },
                    }),
                };
                tokens.expect(Token::is_comma, "`,`")?;
                let value = caller.expr(tokens)?.value;
                tokens.expect(Token::is_right_paren, "`)`")?;
                let block = match &*self.value {
                    Block::Source(src) => Rc::new(Block::Source(SourceBlock {
                        closure: {
                            let env = src.closure.scope();
                            env.define_raw(&ident, value);
                            env
                        },
                        tokens: src.tokens.clone(),
                    })),
                    Block::Intrinsic(_) => self.value.clone(),
                };
                Ok(MunchOutput::Done {
                    block: Rc::new(Block::Intrinsic(Intrinsic::Value(Value::Block(block)))),
                })
            }
            _ => return Err(MunchOutput::Failed {
                error: tokens.error("expected block method"),
            }),
        }
    }
}

pub(crate) struct StringMuncher {
    pub(crate) value: Rc<[char]>,
}

impl Muncher for StringMuncher {
    fn munch_inner(
        &self,
        tokens: &mut Tokens,
        _env: &Env,
        caller: &mut Interpreter,
    ) -> Result<MunchOutput, MunchOutput> {
        tokens.expect(Token::is_dot, "`.`").map_err(MunchOutput::failed)?;

        fn cast_int(value: Value, span: Span) -> Result<i64, Error> {
            if let Value::Int(i) = value {
                Ok(i)
            } else {
                Err(Error {
                    msg: format!("expected int, got {}", value.type_name()),
                    span,
                    notes: Vec::new(),
                })
            }
        }

        if tokens.check(|t| &*t.source == "eq").is_some() {
            tokens.expect(Token::is_left_paren, "`(`")?;
            let arg = caller.expr(tokens)?;
            let result = if let Value::String(s) = &arg.value {
                self.value == *s
            } else {
                return Err(MunchOutput::Failed {
                    error: Error {
                        msg: format!("expected string, got {}", arg.value.type_name()),
                        span: arg.span,
                        notes: Vec::new(),
                    },
                });
            };
            tokens.expect(Token::is_right_paren, "`)`")?;
            Ok(MunchOutput::Done {
                block: Rc::new(Block::Intrinsic(Intrinsic::Value(Value::Bool(result))))
            })
        } else if tokens.check(|t| &*t.source == "len").is_some() {
            Ok(MunchOutput::Done {
                block: Rc::new(Block::Intrinsic(Intrinsic::Value(Value::Int(self.value.len() as i64))))
            })
        } else if tokens.check(|t| &*t.source == "substr").is_some() {
            tokens.expect(Token::is_left_paren, "`(`")?;
            let arg = caller.expr(tokens)?;
            let start = cast_int(arg.value, arg.span)?;
            tokens.expect(Token::is_comma, "`,`")?;
            let arg = caller.expr(tokens)?;
            let end = cast_int(arg.value, arg.span)?;
            tokens.expect(Token::is_right_paren, "`)`")?;
            let start = start.clamp(0, self.value.len() as i64) as usize;
            let end = end.clamp(0, self.value.len() as i64) as usize;
            let end = std::cmp::max(start, end);
            let result = self.value[start..end].into();
            Ok(MunchOutput::Done {
                block: Rc::new(Block::Intrinsic(Intrinsic::Value(Value::String(result))))
            })
        } else if tokens.check(|t| &*t.source == "concat").is_some() {
            tokens.expect(Token::is_left_paren, "`(`")?;
            let arg = caller.expr(tokens)?;
            let result = if let Value::String(s) = &arg.value {
                self.value.iter().chain(s.iter()).copied().collect::<Rc<[char]>>()
            } else {
                return Err(MunchOutput::Failed {
                    error: Error {
                        msg: format!("expected string, got {}", arg.value.type_name()),
                        span: arg.span,
                        notes: Vec::new(),
                    },
                });
            };
            tokens.expect(Token::is_right_paren, "`)`")?;
            Ok(MunchOutput::Done {
                block: Rc::new(Block::Intrinsic(Intrinsic::Value(Value::String(result))))
            })
        } else {
            Err(MunchOutput::Failed {
                error: tokens.error("expected string method"),
            })
        }
    }
}
