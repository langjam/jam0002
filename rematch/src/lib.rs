use ast::Decl;
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};
use thiserror::Error;

pub mod ast;
pub mod compile;
pub mod loc;
pub mod parser;
pub mod repl;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("Unknown binding 'x{name}'")]
    UnknownBinding { name: u32 },
    #[error("Unknown declaration '{name}'")]
    UnknownDeclaration { name: String },
    #[error("Cannot match '{value}' with any clause")]
    MatchClauseNotFound { value: Ast },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    PatHole,
    PatVar { name: u32 },
    PatConstr { name: String, args: Vec<Pat> },
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pat::PatHole => write!(f, "*"),
            Pat::PatVar { name } => write!(f, "?x{}", *name),
            Pat::PatConstr { name, args } => {
                write!(f, "{}(", name)?;
                if args.len() != 0 {
                    write!(f, "{}", args[0])?;
                    for i in &args[1..] {
                        write!(f, ", ")?;
                        i.fmt(f)?
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Pat {
    /// Computes the set of binding variables in a pattern
    pub fn binders(&self) -> Vec<u32> {
        match self {
            Pat::PatHole => vec![],
            Pat::PatVar { name } => vec![*name],
            Pat::PatConstr { args, .. } => args.iter().map(Pat::binders).flatten().collect(),
        }
    }

    /// Rename all the binders occurring in a pattern by increasing them of a given offset
    pub fn rename(&self, offset: u32) -> Self {
        match self {
            Pat::PatHole => Pat::PatHole,
            Pat::PatVar { name } => Pat::PatVar {
                name: *name + offset,
            },
            Pat::PatConstr { name, args } => Pat::PatConstr {
                name: name.clone(),
                args: args.iter().map(|p| p.rename(offset)).collect(),
            },
        }
    }

    pub fn match_with(&self, term: Ast) -> Option<Substitution> {
        match self {
            Pat::PatHole => Some(Substitution::new()),
            Pat::PatVar { name } => Some(Substitution::new().bind(*name, term)),
            Pat::PatConstr { name, args } => match term {
                Ast::Constr {
                    name: real_name,
                    args: real_args,
                } => {
                    if *name == real_name && args.len() == real_args.len() {
                        let s = Substitution::new();
                        args.iter().zip(real_args).try_fold(s, |s, (pat, term)| {
                            pat.match_with(term).map(|s2| s.extend(s2))
                        })
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}

#[derive(Clone)]
pub struct Substitution {
    map: HashMap<u32, Ast>,
    next_var: u32,
}

impl Substitution {
    fn new() -> Self {
        Substitution {
            map: HashMap::new(),
            next_var: 0,
        }
    }

    fn bind(&self, var: u32, term: Ast) -> Self {
        let free_vars_offset = term.fresh_var();
        let mut new_map = self.map.clone();
        let _ = new_map.insert(var, term);
        Substitution {
            map: new_map,
            next_var: u32::max(self.next_var, free_vars_offset),
        }
    }

    fn ignore(&self, varlist: &[u32]) -> Self {
        let mut new_map = self.map.clone();
        varlist.iter().for_each(|v| {
            let _ = new_map.remove_entry(v);
        });
        Substitution {
            map: new_map,
            next_var: self.next_var,
        }
    }

    fn subst(&self, var: u32) -> Ast {
        if let Some(x) = self.map.get(&var) {
            x.clone()
        } else {
            Ast::Var { name: var }
        }
    }

    fn renaming(varlist: &[u32], offset: u32) -> Self {
        let mut map = HashMap::<u32, Ast>::new();
        let next_var = varlist.into_iter().fold(0, |next, v| {
            let _ = map.insert(*v, Ast::Var { name: v + offset });
            u32::max(next, v + offset + 1)
        });
        Substitution { map, next_var }
    }

    fn extend(self, s: Self) -> Self {
        Substitution {
            map: self.map.into_iter().chain(s.map.into_iter()).collect(),
            next_var: u32::max(s.next_var, self.next_var),
        }
    }
}

// function sum_list
// case
//  (case (1, (2, Nil)) of
//  | Nil match Nil
//  | (x, Nil) match (x, Nil)
//  | (x, (y, xs)) rematch (x + y, xs)) of
//  | Nil match 0
//  | (x, Nil) match x

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Var { name: u32 },
    DeclRef { name: String },
    Match { on: Box<Ast>, clauses: Vec<Clause> },
    Constr { name: String, args: Vec<Ast> },
}

struct DisplayAst {
    offset: usize,
    ast: Ast,
}

impl Display for DisplayAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ast {
            Ast::Var { name } => write!(f, "x{}", name),
            Ast::Match { on, clauses } => {
                write!(f, "match {} with\n", on)?;
                for cl in clauses {
                    write!(
                        f,
                        "{:width$}| {} -> {}\n",
                        "",
                        cl.pattern,
                        DisplayAst {
                            offset: self.offset + 4,
                            ast: cl.body.clone()
                        },
                        width = self.offset,
                    )?;
                }
                write!(f, "{:width$}end", "", width = self.offset)
            }
            Ast::DeclRef { name } => write!(f, "{}", name),
            Ast::Constr { name, args } => {
                write!(f, "{}(", name)?;
                if args.len() != 0 {
                    write!(f, "{}", args[0])?;
                    for i in &args[1..] {
                        write!(f, ", ")?;
                        i.fmt(f)?
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            DisplayAst {
                offset: 0,
                ast: self.clone()
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Clause {
    pattern: Pat,
    body: Ast,
}

impl Clause {
    fn run(&self, matched: Ast) -> Option<Ast> {
        log::trace!(
            "Running clause `{} match {}` on `{}`",
            self.pattern,
            self.body,
            matched
        );
        if let Some(s) = self.pattern.match_with(matched) {
            Some(self.body.parallel_subst(&s))
        } else {
            None
        }
    }

    fn rename_binders(&self, offset: u32) -> Clause {
        let binders = self.pattern.binders();
        let body = self.body.rename(&binders, offset);
        let pattern = self.pattern.rename(offset);
        Clause { body, pattern }
    }

    /// Perform safe parallel substitution of a frees variables by terms in
    /// a clause. The binders of the clause are renamed to avoid unwanted
    /// captures of free variables occurring in the term to substitute
    fn parallel_subst_in_body(&self, s: &Substitution) -> Clause {
        // We list the bounded variables in the clause
        let binders = self.pattern.binders();
        // Bounded variables should not be substituted
        let s = s.ignore(&binders);
        // We rename the binders and the bounded variables with fresh names
        // to avoid captures
        let clause = self.rename_binders(s.next_var);
        // We perform the substitution
        let body = self.body.parallel_subst(&s);
        Clause { body, ..clause }
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

    fn fresh_var(&self) -> u32 {
        self.free_vars().iter().max().unwrap_or(&0) + 1
    }

    fn parallel_subst(&self, s: &Substitution) -> Self {
        match self {
            Ast::Var { name } => s.subst(*name),
            Ast::DeclRef { .. } => self.clone(),
            Ast::Match { on, clauses } => Ast::Match {
                on: Box::new((*on).parallel_subst(s)),
                clauses: clauses
                    .iter()
                    .map(|cl| cl.parallel_subst_in_body(s))
                    .collect(),
            },
            Ast::Constr { name, args } => Ast::Constr {
                name: name.clone(),
                args: args.iter().map(|term| term.parallel_subst(s)).collect(),
            },
        }
    }

    /// Perform safe substitution of a variable by a term in a term.
    /// Binders are renamed on-the-fly to avoid unwanted captures.
    fn subst(&self, var: u32, term: Ast) -> Self {
        let s = Substitution::new().bind(var, term);
        self.parallel_subst(&s)
    }

    fn rename(&self, varlist: &[u32], offset: u32) -> Self {
        let renaming = Substitution::renaming(varlist, offset);
        self.parallel_subst(&renaming)
    }

    pub fn run(self, ctx: &AstCtx) -> Result<Self, RuntimeError> {
        match self {
            Ast::Var { .. } => Ok(self),
            Ast::DeclRef { name } => {
                let ast = ctx
                    .decl(&name)
                    .cloned()
                    .ok_or(RuntimeError::UnknownDeclaration { name })?;
                ast.run(ctx)
            }
            Ast::Match { on, clauses } => {
                log::trace!("Matching on {}", on);
                let arg = on.clone().run(ctx)?;
                clauses
                    .into_iter()
                    .find_map(|cl| cl.run(arg.clone()))
                    .ok_or(RuntimeError::MatchClauseNotFound { value: *on })
            }
            Ast::Constr { name, args } => {
                let args = args
                    .into_iter()
                    .map(|ast| ast.run(ctx))
                    .collect::<Result<_, _>>()?;
                Ok(Ast::Constr {
                    name: name.clone(),
                    args,
                })
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct AstCtx {
    decls: BTreeMap<String, Ast>,
    bindings: BTreeMap<u32, String>,
    next_free: u32,
}

impl AstCtx {
    pub fn binding<V: AsRef<u32>>(&self, var: V) -> Option<&str> {
        self.bindings.get(var.as_ref()).map(|s| s.as_str())
    }

    pub fn decl(&self, name: &str) -> Option<&Ast> {
        self.decls.get(name)
    }

    pub fn add_decl(&mut self, decl: Decl) {
        let ast = self.make_expr(decl.body.value);
        self.decls.insert(decl.name.value, ast);
    }

    pub fn make_expr(&mut self, ast: ast::Expr) -> Ast {
        self.make_expr_with_binds(ast, &BTreeMap::default())
    }

    fn make_expr_with_binds(&mut self, ast: ast::Expr, binders: &BTreeMap<String, u32>) -> Ast {
        match ast {
            ast::Expr::Pattern(p) => self.make_pat_expr(p, binders),
            ast::Expr::Binop { op, lhs, rhs } => Ast::Constr {
                name: format!("{:?}", op.value),
                args: vec![
                    self.make_expr_with_binds((*lhs.value).clone(), binders),
                    self.make_expr_with_binds((*rhs.value).clone(), binders),
                ],
            },
            ast::Expr::Match { on, arms } => Ast::Match {
                on: Box::new(self.make_expr_with_binds((*on.value).clone(), binders)),
                clauses: arms.into_iter().map(|cl| self.make_clause(cl)).collect(),
            },
        }
    }

    fn make_clause(&mut self, clause: ast::Clause) -> Clause {
        let (pattern, binders) = self.make_pattern(clause.pattern.value);
        Clause {
            // recursive: clause.recursive,
            body: self.make_expr_with_binds(clause.body.value, &binders),
            pattern,
        }
    }

    fn make_pattern(&mut self, pattern: ast::Pattern) -> (Pat, BTreeMap<String, u32>) {
        match pattern {
            ast::Pattern::Var(s) if &s == "_" => (Pat::PatHole, BTreeMap::default()),
            ast::Pattern::Var(name) => {
                let var = self.next_binding(name.clone());
                let mut map = BTreeMap::new();
                map.insert(name, var);
                (Pat::PatVar { name: var }, map)
            }
            ast::Pattern::Constructor { name, args } => {
                let (args, bindings_iter): (Vec<_>, Vec<_>) = args
                    .into_iter()
                    .map(|pat| self.make_pattern(pat.value))
                    .unzip();
                let bindings = bindings_iter
                    .into_iter()
                    .fold(BTreeMap::new(), |mut map, b| {
                        map.extend(b);
                        map
                    });
                let pat = Pat::PatConstr {
                    name: name.value,
                    args,
                };
                (pat, bindings)
            }
        }
    }

    fn make_pat_expr(&self, prim: ast::Pattern, binders: &BTreeMap<String, u32>) -> Ast {
        match prim {
            ast::Pattern::Var(name) => {
                if let Some(&name) = binders.get(&name) {
                    Ast::Var { name }
                } else {
                    Ast::DeclRef { name }
                }
            }
            ast::Pattern::Constructor { name, args } => Ast::Constr {
                name: name.value,
                args: args
                    .into_iter()
                    .map(|pat| self.make_pat_expr(pat.value, binders))
                    .collect(),
            },
        }
    }

    fn next_binding(&mut self, name: String) -> u32 {
        self.bindings.insert(self.next_free, name);
        let v = self.next_free;
        self.next_free += 1;
        v
    }
}

#[cfg(test)]
mod test {
    use crate::AstCtx;

    use super::Ast::*;
    use super::Clause;
    use super::Pat::*;

    #[static_init::dynamic]
    static EMPTYCTX: AstCtx = AstCtx::default();

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
        let res = prog.run(&EMPTYCTX).unwrap();
        println!("{:?}", res);
        assert_eq!(res, target);
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
        let res = prog.run(&EMPTYCTX).unwrap();
        println!("{:?}", res);
        assert_eq!(res, target);
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
        let res = prog.run(&EMPTYCTX);
        println!("{:?}", res);
        assert!(res.is_err());
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
        let res = prog.run(&EMPTYCTX).unwrap();
        println!("{:?}", res);
        assert_eq!(res, target);
    }

    #[test]
    pub fn test5() {
        // match x2 with x1 -> x0
        let inner = Match {
            on: Box::new(Var { name: 2 }),
            clauses: vec![Clause {
                pattern: PatVar { name: 1 },
                body: Var { name: 0 },
            }],
        };
        // match x1 with x0 -> match x2 -> x0
        let outer = Match {
            on: Box::new(Var { name: 1 }),
            clauses: vec![Clause {
                pattern: PatVar { name: 0 },
                body: inner,
            }],
        };
        let res = outer.clone().run(&EMPTYCTX).unwrap();
        println!("{}", outer.clone());
        println!("{}", res);
        let target = Match {
            on: Box::new(Var { name: 2 }),
            clauses: vec![Clause {
                pattern: PatVar { name: 3 },
                body: Var { name: 1 },
            }],
        };
        assert_eq!(res, target)
    }
}
