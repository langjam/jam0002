use std::collections::HashMap;

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
    Match { on: Box<Ast>, clauses: Vec<Clause> },
    Constr { name: String, args: Vec<Ast> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Clause {
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
            Ast::Match { on, clauses } => Ast::Match {
                on: Box::new((*on).rename(only, offset)),
                clauses: clauses
                    .iter()
                    .map(|cl| Clause {
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

    fn run(self) -> Option<Self> {
        match self {
            Ast::Var { .. } => Some(self),
            Ast::Match { on, clauses } => {
                let arg = on.run()?;
                clauses
                    .into_iter()
                    .find_map(|cl| cl.match_with(arg.clone()))
            }
            Ast::Constr { name, args } => {
                let args = args.into_iter().map(Ast::run).collect::<Option<_>>()?;
                Some(Ast::Constr {
                    name: name.clone(),
                    args,
                })
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Ast::*;
    use super::Clause;
    use super::Pat::*;

    #[test]
    pub fn test1() {
        let prog = Match {
            on: Box::new(Var { name: 1 }),
            clauses: vec![Clause {
                pattern: PatVar { name: 0 },
                body: Var { name: 0 },
            }],
        };
        let target = Var { name: 1 };
        let res = prog.run();
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
                pattern: PatConstr {
                    name: "MyConstr".to_string(),
                    args: vec![],
                },
                body: Var { name: 1 },
            }],
        };
        let target = Var { name: 1 };
        let res = prog.run();
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
                pattern: PatConstr {
                    name: "MyConstr".to_string(),
                    args: vec![PatVar { name: 1 }],
                },
                body: Var { name: 1 },
            }],
        };
        let res = prog.run();
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
        let res = prog.run();
        println!("{:?}", res);
        assert_eq!(res, Some(target))
    }
}

fn main() {
    println!("hello")
}
