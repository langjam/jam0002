pub mod ast;
pub mod loc;
pub mod parser;

use std::collections::HashMap;

use loc::Located;

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    PatVar { name: u32 },
    PatConstr { name: String, args: Vec<Pat> },
}

impl Pat {
    /// Computes the set of binding variables in a pattern
    fn binders(&self) -> Vec<u32> {
        match self {
            Pat::PatVar { name } => vec![*name],
            Pat::PatConstr { args, .. } => args.iter().map(Pat::binders).flatten().collect(),
        }
    }

    /// Rename all the binders occurring in a pattern by increasing them of a given offset
    fn rename(&self, offset: u32) -> Self {
        match self {
            Pat::PatVar { name } => Pat::PatVar {
                name: *name + offset,
            },
            Pat::PatConstr { name, args } => Pat::PatConstr {
                name: name.clone(),
                args: args.iter().map(|p| p.rename(offset)).collect(),
            },
        }
    }

    fn match_with(&self, term: Ast, h: &mut HashMap<u32, Ast>) -> bool {
        match self {
            Pat::PatVar { name } => match h.insert(*name, term) {
                Some(_) => panic!("multiple variables with the same name in a pattern"),
                None => true,
            },
            Pat::PatConstr { name, args } => match term {
                Ast::Constr {
                    name: real_name,
                    args: real_args,
                } => {
                    if *name == real_name && args.len() == real_args.len() {
                        args.iter()
                            .zip(real_args)
                            .all(|(pat, term)| pat.match_with(term, h))
                    } else {
                        false
                    }
                }
                _ => false,
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Var { name: u32 },
    DeclRef { name: String },
    Match { on: Box<Ast>, clauses: Vec<Clause> },
    Constr { name: String, args: Vec<Ast> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Clause {
    recursive: bool,
    pattern: Pat,
    body: Ast,
}

impl Clause {
    /// Perform safe substitution of a free variable by a term in
    /// a clause. The binders of the clause are renamed to avoid unwanted
    /// captures of free variables occurring in the term to substitute
    fn subst_in_body(&self, var: u32, term: Ast) -> Self {
        // We list the binders
        let binders = self.pattern.binders();
        // We list the free variables occurring in the term
        // to substitute
        let free_vars = term.free_vars();
        // We compute a minimal offset to generate fresh variables
        let fresh_vars_offset = *free_vars.iter().max().unwrap_or(&0) + 1;
        // We rename the binding variables of the clause to avoid capturing
        // free variables occurring in the term to substitute and then we safely perform
        // substitution
        Clause {
            recursive: false,
            pattern: self.pattern.rename(fresh_vars_offset),
            body: self
                .body
                .clone()
                .rename(&binders, fresh_vars_offset)
                .subst(var, term),
        }
    }

    fn match_with(&self, matched: Ast) -> Option<Ast> {
        let mut h = HashMap::<u32, Ast>::new();
        if self.pattern.match_with(matched, &mut h) {
            Some(
                h.into_iter()
                    .fold(self.body.clone(), |body, (x, valx)| body.subst(x, valx)),
            )
        } else {
            None
        }
    }
}

impl Ast {
    /// Computes the set of free variables occurring in
    /// a term.
    /// Free variables listed in 'except' are excluded from the search.
    fn free_vars_except(&self, except: &[u32]) -> Vec<u32> {
        match self {
            Ast::Var { name } => {
                if except.contains(name) {
                    vec![]
                } else {
                    vec![*name]
                }
            }
            Ast::DeclRef { .. } => vec![],
            Ast::Match { on, clauses } => {
                let vars_on = on.free_vars_except(except);
                let var_clauses = clauses
                    .iter()
                    .map(|cl| cl.body.free_vars_except(&cl.pattern.binders()))
                    .flatten();
                var_clauses.chain(vars_on.into_iter()).collect()
            }
            Ast::Constr { args, .. } => args
                .iter()
                .map(|ast| ast.free_vars_except(except))
                .flatten()
                .collect(),
        }
    }

    /// Computes the set of free variables occurring in a term
    fn free_vars(&self) -> Vec<u32> {
        self.free_vars_except(&[])
    }

    /// Perform safe substitution of a variable by a term in a term.
    /// Binders are renamed on-the-fly to avoid unwanted captures.
    fn subst(&self, var: u32, term: Ast) -> Self {
        match self {
            Ast::Var { name } => {
                if *name == var {
                    term
                } else {
                    Ast::Var { name: *name }
                }
            }
            Ast::DeclRef { .. } => self.clone(),
            Ast::Match { on, clauses } => Ast::Match {
                on: Box::new((*on).subst(var, term.clone())),
                clauses: clauses
                    .iter()
                    .map(|cl| cl.subst_in_body(var, term.clone()))
                    .collect(),
            },
            Ast::Constr { name, args } => Ast::Constr {
                name: name.clone(),
                args: args.iter().map(|a| a.subst(var, term.clone())).collect(),
            },
        }
    }

    fn rename(self, only: &[u32], offset: u32) -> Self {
        match self {
            Ast::Var { name } => {
                println!("renaming {} -> {}", name, name + offset);
                if only.contains(&name) {
                    Ast::Var {
                        name: name + offset,
                    }
                } else {
                    self
                }
            }
            Ast::DeclRef { .. } => self,
            Ast::Match { on, clauses } => Ast::Match {
                on: Box::new((*on).rename(only, offset)),
                clauses: clauses
                    .iter()
                    .map(|cl| Clause {
                        recursive: cl.recursive,
                        pattern: cl.pattern.clone(),
                        body: cl.body.clone().rename(only, offset),
                    })
                    .collect(),
            },
            Ast::Constr { name, args } => Ast::Constr {
                name,
                args: args
                    .into_iter()
                    .map(|term| term.rename(only, offset))
                    .collect(),
            },
        }
    }

    pub fn run(self, ctx: &AstContext) -> Option<Self> {
        match self {
            Ast::Var { .. } => Some(self),
            Ast::DeclRef { name } => {
                let Located { value: ast, .. } = ctx.declaration(name)?;
                ast.clone().run(ctx)
            }
            Ast::Match { on, clauses } => {
                let arg = on.run(ctx)?;
                clauses
                    .into_iter()
                    .find_map(|cl| cl.match_with(arg.clone()))
            }
            Ast::Constr { name, args } => {
                let args = args
                    .into_iter()
                    .map(|x| x.run(ctx))
                    .collect::<Option<_>>()?;
                Some(Ast::Constr {
                    name: name.clone(),
                    args,
                })
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AstContext {
    decls: HashMap<String, Located<Ast>>,
    binding_names: HashMap<u32, String>,
    next_free: u32,
}

impl AstContext {
    pub fn declaration<S: AsRef<str>>(&self, name: S) -> Option<Located<&Ast>> {
        self.decls.get(name.as_ref()).map(|l| l.as_ref())
    }

    pub fn binding(&self, var: u32) -> Option<&str> {
        self.binding_names.get(&var).map(|s| s.as_str())
    }

    pub fn add_decl(&mut self, decl: ast::Decl) {
        let ast = self.make_expr(decl.body);
        self.decls.insert(decl.name.into_inner(), ast);
    }

    pub fn make_expr(&mut self, ast: Located<ast::Expr>) -> Located<Ast> {
        let Located { file_id, span, .. } = ast;
        ast.map(|ast| match ast {
            ast::Expr::Pattern(p) => {
                self.make_pat_expr(Located {
                    file_id,
                    span,
                    value: p,
                })
                .value
            }
            ast::Expr::Binop { op, lhs, rhs } => Ast::Constr {
                name: format!("{:?}", op.into_inner()),
                args: vec![
                    self.make_expr(lhs.as_deref().cloned()).into_inner(),
                    self.make_expr(rhs.as_deref().cloned()).into_inner(),
                ],
            },
            ast::Expr::Match { on, arms } => Ast::Match {
                on: self
                    .make_expr(on.as_deref().cloned())
                    .map(Box::new)
                    .into_inner(),
                clauses: arms
                    .into_iter()
                    .map(|clause| self.make_clause(clause))
                    .collect(),
            },
        })
    }

    pub fn make_clause(&mut self, clause: ast::Clause) -> Clause {
        Clause {
            recursive: clause.recursive,
            body: self.make_expr(clause.body).into_inner(),
            pattern: self.make_pattern(clause.pattern).into_inner(),
        }
    }

    pub fn make_pattern(&mut self, pattern: Located<ast::Pattern>) -> Located<Pat> {
        match pattern.value {
            ast::Pattern::Var(p) => self.make_pat_pattern(Located {
                value: ast::Pattern::Var(p),
                span: pattern.span,
                file_id: pattern.file_id,
            }),
            ast::Pattern::Constructor { name, args } => Located {
                value: Pat::PatConstr {
                    name: name.into_inner(),
                    args: args
                        .into_iter()
                        .map(|pat| self.make_pattern(pat).into_inner())
                        .collect(),
                },
                span: pattern.span,
                file_id: pattern.file_id,
            },
        }
    }

    pub fn make_pat_expr(&mut self, prim: Located<ast::Pattern>) -> Located<Ast> {
        prim.map(|prim| match prim {
            ast::Pattern::Constructor { name, args } => Ast::Constr {
                name: name.into_inner(),
                args: args
                    .into_iter()
                    .map(|pat| self.make_pat_expr(pat).into_inner())
                    .collect(),
            },
            ast::Pattern::Var(v) => Ast::Var {
                name: self.next_var(v),
            },
        })
    }

    pub fn make_pat_pattern(&mut self, prim: Located<ast::Pattern>) -> Located<Pat> {
        prim.map(|prim| match prim {
            ast::Pattern::Constructor { name, args } => Pat::PatConstr {
                name: name.into_inner(),
                args: args
                    .into_iter()
                    .map(|pat| self.make_pat_pattern(pat).into_inner())
                    .collect(),
            },
            // ast::Pattern::Var(v) if &v == "_" => Pat::PatHole,
            ast::Pattern::Var(v) => Pat::PatVar {
                name: self.next_var(v),
            },
        })
    }

    fn next_var(&mut self, var: String) -> u32 {
        self.binding_names.insert(self.next_free, var);
        let v = self.next_free;
        self.next_free += 1;
        v
    }
}

#[cfg(test)]
mod test {
    use crate::AstContext;

    use super::Ast::*;
    use super::Clause;
    use super::Pat::*;

    #[test]
    pub fn test1() {
        let prog = Match {
            on: Box::new(Var { name: 1 }),
            clauses: vec![Clause {
                recursive: false,
                pattern: PatVar { name: 0 },
                body: Var { name: 0 },
            }],
        };
        let target = Var { name: 1 };
        let res = prog.run(&AstContext::default());
        println!("{:?}", res);
        assert_eq!(res, Some(target))
    }

    #[test]
    pub fn test2() {
        let prog = Match {
            on: Box::new(Constr {
                name: "MyConstr".to_string(),
                args: vec![],
            }),
            clauses: vec![Clause {
                recursive: false,
                pattern: PatConstr {
                    name: "MyConstr".to_string(),
                    args: vec![],
                },
                body: Var { name: 1 },
            }],
        };
        let target = Var { name: 1 };
        let res = prog.run(&AstContext::default());
        println!("{:?}", res);
        assert_eq!(res, Some(target))
    }

    #[test]
    pub fn test3() {
        let prog = Match {
            on: Box::new(Constr {
                name: "MyConstr".to_string(),
                args: vec![],
            }),
            clauses: vec![Clause {
                recursive: false,
                pattern: PatConstr {
                    name: "MyConstr".to_string(),
                    args: vec![PatVar { name: 1 }],
                },
                body: Var { name: 1 },
            }],
        };
        let res = prog.run(&AstContext::default());
        println!("{:?}", res);
        assert_eq!(res, None)
    }

    #[test]
    pub fn test4() {
        let cons = "Cons".to_string();
        let prog = Match {
            on: Box::new(Constr {
                name: cons.clone(),
                args: vec![
                    Var { name: 1 },
                    Constr {
                        name: cons.clone(),
                        args: vec![
                            Var { name: 2 },
                            Constr {
                                name: "Nil".to_string(),
                                args: vec![],
                            },
                        ],
                    },
                ],
            }),
            clauses: vec![Clause {
                recursive: false,
                pattern: PatConstr {
                    name: cons.clone(),
                    args: vec![
                        PatVar { name: 0 },
                        PatConstr {
                            name: cons.clone(),
                            args: vec![
                                PatVar { name: 1 },
                                PatConstr {
                                    name: "Nil".to_string(),
                                    args: vec![],
                                },
                            ],
                        },
                    ],
                },
                body: Constr {
                    name: "First2".to_string(),
                    args: vec![Var { name: 0 }, Var { name: 1 }],
                },
            }],
        };
        let target = Constr {
            name: "First2".to_string(),
            args: vec![Var { name: 1 }, Var { name: 2 }],
        };
        let res = prog.run(&AstContext::default());
        println!("{:?}", res);
        assert_eq!(res, Some(target))
    }
}
