#[derive(Clone, Debug)]
pub enum Pat {
    PatVar { name: u32 },
    PatConstr { name: String, args: Vec<Pat> },
}

#[derive(Clone, Debug)]
pub enum Ast {
    Var {
        name: u32,
    },
    Match {
        on: Box<Ast>,
        patterns: Vec<(Pat, Ast)>,
    },
    Constr {
        name: String,
        args: Vec<Ast>,
    },
}

impl Ast {
    fn free_vars(&self, except: &[u32]) -> Vec<u32> {
        match self {
            Ast::Var { name } => vec![*name],
            Ast::Match { on, patterns } => on
                .free_vars(except)
                .into_iter()
                .chain(
                    patterns
                        .into_iter()
                        .map(|(pat, ast)| {
                            ast.free_vars(&[except, pat.vars().as_ref()].concat::<u32>())
                        })
                        .flatten(),
                )
                .collect(),

            Ast::Constr { args, .. } => {
                args.iter().map(|a| a.free_vars(except)).flatten().collect()
            }
        }
    }

    fn fresh_var(&self) -> u32 {
        self.free_vars(&[]).iter().max().unwrap_or(&0) + 1
    }

    fn subst(self, name: u32, arg: Ast) -> Self {
        match self {
            Ast::Var { name: x } if x == name => arg,
            Ast::Var { .. } => self,
            Ast::Match { on, patterns } => Ast::Match {
                on: Box::new((*on).subst(name, arg.clone())),
                patterns: patterns
                    .into_iter()
                    .map(|(p, ast)| {
                        if p.vars().contains(&name) {
                            (p, ast.subst(name, arg.clone()))
                        } else {
                            (p, ast)
                        }
                    })
                    .collect(),
            },
            Ast::Constr { name: x, args } => Ast::Constr {
                name: x,
                args: args
                    .into_iter()
                    .map(|ast| ast.subst(name, arg.clone()))
                    .collect(),
            },
        }
    }

    fn rename(self, except: &[u32], u: u32) -> Self {
        match self {
            Ast::Var { name } => {
                if except.contains(&name) {
                    self
                } else {
                    Ast::Var { name: name + u }
                }
            }
            Ast::Match { on, patterns } => Ast::Match {
                on: Box::new((*on).rename(except, u)),
                patterns: patterns
                    .into_iter()
                    .map(|(pat, ast)| (pat.clone(), ast.rename(&pat.vars(), u)))
                    .collect(),
            },
            Ast::Constr { name, args } => Ast::Constr {
                name,
                args: args.into_iter().map(|ast| ast.rename(except, u)).collect(),
            },
        }
    }

    fn run(self) -> Option<Self> {
        match self {
            Ast::Var { .. } => Some(self),
            Ast::Match { on, patterns } => {
                if patterns.len() == 0 {
                    None
                } else {
                    patterns.into_iter().find_map(|(pat, ast)| {
                        let offset = on.fresh_var();
                        pat.rename(offset)
                            .do_match_one(*on.clone(), ast.rename(&[], offset))
                    })
                }
            }
            Ast::Constr { name, args } => {
                let args = args.into_iter().map(Ast::run).collect::<Option<_>>()?;
                Some(Ast::Constr { name, args })
            }
        }
    }
}

impl Pat {
    fn vars(&self) -> Vec<u32> {
        match self {
            Pat::PatVar { name } => vec![*name],
            Pat::PatConstr { args, .. } => args.iter().map(|p| p.vars()).flatten().collect(),
        }
    }

    fn rename(&self, u: u32) -> Self {
        match self {
            Pat::PatVar { name } => Pat::PatVar { name: name + u },
            Pat::PatConstr { name, args } => Pat::PatConstr {
                name: name.clone(),
                args: args.iter().map(|p| p.rename(u)).collect(),
            },
        }
    }

    fn do_match_one(&self, matched: Ast, body: Ast) -> Option<Ast> {
        match self {
            Pat::PatVar { name } => Some(body.subst(*name, matched.clone())),
            Pat::PatConstr { name, args } => match matched {
                Ast::Var { .. } => None,
                Ast::Match { .. } => None,
                Ast::Constr {
                    name: name2,
                    args: args2,
                } => {
                    if *name == *name2 && args.len() == args2.len() {
                        args.iter()
                            .zip(args2.iter())
                            .try_fold(body.clone(), |body, (pat, ast)| {
                                pat.do_match_one((*ast).clone(), body)
                            })
                    } else {
                        None
                    }
                }
            },
        }
    }
}

fn main() {
    use Ast::*;

    // let prog =
    //  match MyConstr(x1) with
    //  | MyConstr(x0) -> x0
    //
    // Should return 'x1'
    let prog = Match {
        on: Box::new(Constr {
            name: "MyConstr".to_string(),
            args: vec![Var { name: 1 }],
        }),
        patterns: vec![(
            Pat::PatConstr {
                name: "MyConstr".to_string(),
                args: vec![Pat::PatVar { name: 0 }],
            },
            Var { name: 0 },
        )],
    };
    println!("{:?}", prog.run());

    let prog2 = Match {
        on: Box::new(Constr {
            name: "Pair".to_string(),
            args: vec![Var { name: 1 }, Var { name: 2 }],
        }),
        patterns: vec![(
            Pat::PatConstr {
                name: "Pair".to_string(),
                args: vec![Pat::PatVar { name: 0 }, Pat::PatVar { name: 1 }],
            },
            Constr {
                name: "Result".to_string(),
                args: vec![Var { name: 0 }, Var { name: 1 }],
            },
        )],
    };
    println!("{:?}", prog2.run());
}
