pub mod ast;
pub mod loc;
pub mod parser;

use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, LabelStyle};
use loc::{Located, Span};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

#[derive(Debug, Clone, thiserror::Error)]
pub enum ExecutionError {
    #[error("Unknown binding")]
    UnknownBinding,
    #[error("Unexpected arm in case")]
    UnexpectedCase { ctx: AstContext, on: Located<Ast> },
}

impl ExecutionError {
    pub fn make_diagnostic(this: Located<Self>) -> Diagnostic<FileId> {
        let mut labels = vec![this.clone().to_label(LabelStyle::Primary)];
        match &this.value {
            Self::UnexpectedCase { ctx, on } => labels.push(
                on.as_ref()
                    .map(|ast| {
                        format!(
                            "This case expression does not have an arm for `{}`",
                            ast.display(&ctx)
                        )
                    })
                    .to_label(LabelStyle::Secondary),
            ),
            _ => {}
        }
        Diagnostic::error()
            .with_message(this.value.to_string())
            .with_labels(labels)
    }
}

pub type ExecR<T> = Result<T, Located<ExecutionError>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    PatHole,
    PatVar {
        name: Located<u32>,
    },
    PatConstr {
        name: Located<String>,
        args: Vec<Located<Self>>,
    },
}

impl Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pat::PatHole => write!(f, "*"),
            Pat::PatVar {
                name: Located { value: name, .. },
            } => write!(f, "?x{}", name),
            Pat::PatConstr {
                name: Located { value: name, .. },
                args,
            } => {
                write!(f, "{}(", name)?;
                if args.len() != 0 {
                    write!(f, "{}", *args[0])?;
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
    pub fn binders(&self) -> BTreeSet<Located<u32>> {
        match self {
            Pat::PatHole => BTreeSet::default(),
            Pat::PatVar { name } => {
                let mut set = BTreeSet::default();
                set.insert(name.as_ref().copied());
                set
            }
            Pat::PatConstr { args, .. } => args
                .iter()
                .map(|p| p.as_ref().into_inner().binders())
                .flatten()
                .collect(),
        }
    }

    /// Rename all the binders occurring in a pattern by increasing them of a given offset
    pub fn rename(&self, offset: u32) -> Self {
        match self {
            Pat::PatHole => Pat::PatHole,
            Pat::PatVar { name } => Pat::PatVar {
                name: name.as_ref().map(|&n| n + offset),
            },
            Pat::PatConstr { name, args } => Pat::PatConstr {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|p| p.as_ref().map(|p| p.rename(offset)))
                    .collect(),
            },
        }
    }

    pub fn match_with(&self, term: Located<Ast>) -> Option<Substitution> {
        match self {
            Pat::PatHole => Some(Substitution::new()),
            Pat::PatVar {
                name: Located { value: name, .. },
            } => Some(Substitution::new().bind(*name, term)),
            Pat::PatConstr {
                name: Located { value: name, .. },
                args,
            } => match term.into_inner() {
                Ast::Constr {
                    name:
                        Located {
                            value: real_name, ..
                        },
                    args: real_args,
                } => {
                    if *name == real_name && args.len() == real_args.len() {
                        let s = Substitution::new();
                        args.iter().zip(real_args).try_fold(s, |s, (pat, term)| {
                            pat.as_ref()
                                .into_inner()
                                .match_with(term)
                                .map(|s2| s.extend(s2))
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
    map: HashMap<u32, Located<Ast>>,
    next_var: u32,
}

impl Substitution {
    fn new() -> Self {
        Substitution {
            map: HashMap::new(),
            next_var: 0,
        }
    }

    fn bind(&self, var: u32, term: Located<Ast>) -> Self {
        let free_vars_offset = term.fresh_var();
        let mut new_map = self.map.clone();
        let _ = new_map.insert(var, term);
        Substitution {
            map: new_map,
            next_var: u32::max(self.next_var, free_vars_offset),
        }
    }

    fn ignore(&self, varlist: impl IntoIterator<Item = Located<u32>>) -> Self {
        let mut new_map = self.map.clone();
        varlist.into_iter().for_each(|v| {
            let _ = new_map.remove_entry(&*v);
        });
        Substitution {
            map: new_map,
            next_var: self.next_var,
        }
    }

    fn subst(&self, var: Located<u32>) -> Ast {
        if let Some(x) = self.map.get(var.as_ref().into_inner()) {
            x.as_ref().into_inner().clone()
        } else {
            Ast::Var { name: var }
        }
    }

    fn renaming(varlist: impl IntoIterator<Item = Located<u32>>, offset: u32) -> Self {
        let mut map = HashMap::new();
        let next_var = varlist.into_iter().fold(0, |next, v| {
            let loc = v.as_ref();
            let v = *v.as_ref().into_inner();
            let _ = map.insert(
                v,
                loc.clone().map(|&v| Ast::Var {
                    name: loc.map(|_| v + offset),
                }),
            );
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
    Var {
        name: Located<u32>,
    },
    Match {
        on: Located<Box<Self>>,
        clauses: Vec<Located<Clause>>,
    },
    Constr {
        name: Located<String>,
        args: Vec<Located<Self>>,
    },
}

struct DisplayAst<'ctx, 'ast> {
    offset: usize,
    context: &'ctx AstContext,
    ast: &'ast Ast,
}

impl<'ctx, 'ast> Display for DisplayAst<'ctx, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ast {
            Ast::Var {
                name: Located { value: name, .. },
            } => match self.context.variable(*name) {
                Some(name) => write!(f, "{}", name),
                None => write!(f, "x{}", name),
            },
            Ast::Match { on, clauses } => {
                write!(f, "case {} of\n", on.display(self.context))?;
                for cl in clauses {
                    let Located { value: cl, .. } = cl.as_ref();
                    write!(
                        f,
                        "{:width$}| {} {} {}\n",
                        "",
                        cl.pattern.as_ref().into_inner(),
                        if cl.recursive { "match" } else { "rematch" },
                        DisplayAst {
                            offset: self.offset + 4,
                            context: self.context,
                            ast: &cl.body
                        },
                        width = self.offset,
                    )?;
                }
                write!(f, "{:width$}end", "", width = self.offset)
            }
            Ast::Constr {
                name: Located { value: name, .. },
                args,
            } => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}(", name)?;
                    if args.len() != 0 {
                        write!(f, "{}", args[0].display(self.context))?;
                        for i in &args[1..] {
                            write!(f, ", {}", i.display(self.context))?;
                        }
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Clause {
    recursive: bool,
    pattern: Located<Pat>,
    body: Located<Ast>,
}

impl Clause {
    fn run(&self, matched: Located<Ast>) -> Option<Ast> {
        if let Some(s) = self.pattern.match_with(matched) {
            Some(self.body.parallel_subst(&s))
        } else {
            None
        }
    }

    fn rename_binders(&self, offset: u32) -> Clause {
        let binders = self.pattern.binders();
        let body = self
            .body
            .as_ref()
            .map(|body| body.rename(binders.iter().copied(), offset));
        let pattern = self.pattern.as_ref().map(|pat| pat.rename(offset));
        Clause {
            recursive: false, /* TODO: Make sure this is alright */
            body,
            pattern,
        }
    }

    /// Perform safe parallel substitution of a frees variables by terms in
    /// a clause. The binders of the clause are renamed to avoid unwanted
    /// captures of free variables occurring in the term to substitute
    fn parallel_subst_in_body(&self, s: &Substitution) -> Clause {
        // We list the bounded variables in the clause
        let binders = self.pattern.binders();
        // Bounded variables should not be substituted
        let s = s.ignore(binders.iter().copied());
        // We rename the binders and the bounded variables with fresh names
        // to avoid captures
        let clause = self.rename_binders(s.next_var);
        // We perform the substitution
        let body = self.body.as_ref().map(|body| body.parallel_subst(&s));
        Clause { body, ..clause }
    }
}

impl Ast {
    /// Computes the set of free variables occurring in
    /// a term.
    /// Free variables listed in 'except' are excluded from the search.
    fn free_vars_except(&self, except: &BTreeSet<u32>) -> BTreeSet<Located<u32>> {
        match self {
            Ast::Var { name } => {
                if except.contains(*name.as_ref()) {
                    BTreeSet::default()
                } else {
                    let mut set = BTreeSet::default();
                    set.insert(*name);
                    set
                }
            }
            Ast::Match { on, clauses } => {
                let vars_on = on.free_vars_except(except);
                let var_clauses = clauses
                    .iter()
                    .map(|cl| {
                        let binders = cl
                            .pattern
                            .binders()
                            .into_iter()
                            .map(Located::into_inner)
                            .collect();
                        cl.body.free_vars_except(&binders)
                    })
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
    fn free_vars(&self) -> BTreeSet<Located<u32>> {
        self.free_vars_except(&BTreeSet::default())
    }

    fn fresh_var(&self) -> u32 {
        self.free_vars()
            .iter()
            .map(|loc| loc.into_inner())
            .max()
            .unwrap_or(0)
            + 1
    }

    fn parallel_subst(&self, s: &Substitution) -> Self {
        match self {
            Ast::Var { name } => s.subst(*name),
            Ast::Match { on, clauses } => Ast::Match {
                on: on.as_deref().map(|on| Box::new(on.parallel_subst(s))),
                clauses: clauses
                    .iter()
                    .map(|cl| cl.as_ref().map(|cl| cl.parallel_subst_in_body(s)))
                    .collect(),
            },
            Ast::Constr { name, args } => Ast::Constr {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|term| term.as_ref().map(|term| term.parallel_subst(s)))
                    .collect(),
            },
        }
    }

    /// Perform safe substitution of a variable by a term in a term.
    /// Binders are renamed on-the-fly to avoid unwanted captures.
    fn subst(&self, var: u32, term: Located<Ast>) -> Self {
        let s = Substitution::new().bind(var, term);
        self.parallel_subst(&s)
    }

    fn rename(&self, varlist: impl IntoIterator<Item = Located<u32>>, offset: u32) -> Self {
        let renaming = Substitution::renaming(varlist, offset);
        self.parallel_subst(&renaming)
    }

    pub fn run(&self, ctx: &AstContext) -> ExecR<Self> {
        match self {
            Ast::Var { name } => {
                let var_ast = match ctx.variable(**name).and_then(|name| ctx.binding(name)) {
                    Some(ast) => ast,
                    None => return Err(name.map(|_| ExecutionError::UnknownBinding)),
                };
                var_ast.run(ctx)
            }
            Ast::Match { on, clauses } => {
                let arg = on
                    .as_deref()
                    .map(|on| on.run(ctx))
                    .transpose_result()
                    .map_err(|err| err.flatten())?;
                let (rec, res) = match clauses.into_iter().find_map(|cl| {
                    Some((
                        cl.recursive,
                        cl.as_ref()
                            .map(|cl| cl.run(arg.clone()))
                            .transpose_option()?,
                    ))
                }) {
                    Some(v) => v,
                    None => {
                        let span = clauses.iter().map(Located::span).fold(on.span, Span::merge);
                        return Err(Located {
                            span,
                            file_id: clauses.first().map(Located::file_id).unwrap_or(on.file_id),
                            value: ExecutionError::UnexpectedCase {
                                ctx: ctx.clone(),
                                on: on.as_deref().cloned(),
                            },
                        });
                    }
                };
                if rec {
                    Ast::Match {
                        on: res.map(Box::new),
                        clauses: clauses.clone(),
                    }
                    .run(ctx)
                } else {
                    Ok(res.into_inner())
                }
            }
            Ast::Constr { name, args } => {
                let args = args
                    .iter()
                    .map(|ast| {
                        ast.as_ref()
                            .map(|ast| ast.run(ctx))
                            .transpose_result()
                            .map_err(|err| err.flatten())
                    })
                    .collect::<ExecR<_>>()?;
                Ok(Ast::Constr {
                    name: name.clone(),
                    args,
                })
            }
        }
    }

    pub fn display<'ctx, 'ast: 'ctx>(&'ast self, context: &'ctx AstContext) -> impl 'ctx + Display {
        DisplayAst {
            ast: self,
            context,
            offset: 0,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AstContext {
    var_data: HashMap<String, Located<Ast>>,
    var_ref: HashMap<u32, String>,
    next_free: u32,
}

impl AstContext {
    pub fn binding<S: AsRef<str>>(&self, name: S) -> Option<Located<&Ast>> {
        self.var_data.get(name.as_ref()).map(|l| l.as_ref())
    }

    pub fn variable(&self, var: u32) -> Option<&str> {
        self.var_ref.get(&var).map(|s| s.as_str())
    }

    pub fn add_decl(&mut self, decl: ast::Decl) {
        let ast = self.make_expr(decl.body);
        self.var_data.insert(decl.name.into_inner(), ast);
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
                name: op.map(|binop| format!("{:?}", binop)),
                args: vec![
                    self.make_expr(lhs.as_deref().cloned()),
                    self.make_expr(rhs.as_deref().cloned()),
                ],
            },
            ast::Expr::Match { on, arms } => Ast::Match {
                on: self.make_expr(on.as_deref().cloned()).map(Box::new),
                clauses: arms
                    .into_iter()
                    .map(|clause| {
                        let file_id = clause.body.file_id;
                        let span = clause.pattern.span().merge(clause.body.span());
                        self.make_clause(Located {
                            value: clause,
                            span,
                            file_id,
                        })
                    })
                    .collect(),
            },
        })
    }

    pub fn make_clause(&mut self, clause: Located<ast::Clause>) -> Located<Clause> {
        clause.map(|clause| Clause {
            recursive: clause.recursive,
            body: self.make_expr(clause.body),
            pattern: self.make_pattern(clause.pattern),
        })
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
                    name,
                    args: args.into_iter().map(|pat| self.make_pattern(pat)).collect(),
                },
                span: pattern.span,
                file_id: pattern.file_id,
            },
        }
    }

    pub fn make_pat_expr(&mut self, prim: Located<ast::Pattern>) -> Located<Ast> {
        let Located { file_id, span, .. } = prim;
        prim.map(|prim| match prim {
            ast::Pattern::Constructor { name, args } => Ast::Constr {
                name,
                args: args
                    .into_iter()
                    .map(|pat| self.make_pat_expr(pat))
                    .collect(),
            },
            ast::Pattern::Var(v) => Ast::Var {
                name: Located {
                    value: self.next_var(v),
                    span,
                    file_id,
                },
            },
        })
    }

    pub fn make_pat_pattern(&mut self, prim: Located<ast::Pattern>) -> Located<Pat> {
        let Located { file_id, span, .. } = prim;
        prim.map(|prim| match prim {
            ast::Pattern::Constructor { name, args } => Pat::PatConstr {
                name,
                args: args
                    .into_iter()
                    .map(|pat| self.make_pat_pattern(pat))
                    .collect(),
            },
            ast::Pattern::Var(v) if &v == "_" => Pat::PatHole,
            ast::Pattern::Var(v) => Pat::PatVar {
                name: Located {
                    value: self.next_var(v),
                    span,
                    file_id,
                },
            },
        })
    }

    fn next_var(&mut self, var: String) -> u32 {
        self.var_ref.insert(self.next_free, var);
        let v = self.next_free;
        self.next_free += 1;
        v
    }
}

#[cfg(test)]
mod test {
    use codespan::FileId;
    use codespan_reporting::diagnostic::Diagnostic;

    use crate::loc::Located;
    use crate::parser::parse_repl;
    use crate::parser::ReplParse;
    use crate::Ast;
    use crate::AstContext;

    fn parse_expr(expr: &str) -> Result<(AstContext, Located<Ast>), Diagnostic<FileId>> {
        let mut files = codespan::Files::new();
        let file_id = files.add("<test>", expr);
        let ast = parse_repl(&files, file_id)?;
        let expr = ast.map(|ast| {
            if let ReplParse::Expr(expr) = ast {
                expr
            } else {
                unreachable!()
            }
        });
        let mut ctx = AstContext::default();
        let expr = ctx.make_expr(expr);
        Ok((ctx, expr))
    }

    #[test]
    pub fn test1() {
        let (ctx, prog) = parse_expr("case x of | y match y end").unwrap();
        let res = prog.run(&ctx).unwrap();
        println!("{:?}", res);
        match res {
            Ast::Var {
                name: Located { value, .. },
            } => assert_eq!(1, value),
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test2() {
        let (ctx, prog) = parse_expr("case (MyConstr) of | (MyConstr) match x1 end").unwrap();
        /*         let prog = Match {
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
        }; */
        let res = prog.run(&ctx).unwrap();
        println!("{:?}", res);
        match res {
            Ast::Var {
                name: Located { value, .. },
            } => assert_eq!(1, value),
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test3() {
        let (ctx, prog) = parse_expr("case (MyConstr) of | (MyConstr x) match x end").unwrap();
        /*         let prog = Match {
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
        }; */
        let res = prog.run(&ctx);
        println!("{:?}", res);
        assert!(res.is_err());
    }

    #[test]
    pub fn test4() {
        let (ctx, prog) = parse_expr(
            r"
            case (Cons x1 (Cons x2 Nil)) of
            | (Cons a (Cons b Nil)) match (First2 a b)
            end",
        )
        .unwrap();
        /*         let prog = Match {
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
        }; */
        let res = prog.run(&ctx).unwrap();
        println!("{:?}", res);
        match res {
            Ast::Constr {
                name: Located { value: name, .. },
                args,
            } => {
                assert_eq!("First2", &name);
                assert_eq!(
                    vec![1, 2],
                    args.into_iter()
                        .filter_map(|l| match l.value {
                            Ast::Var {
                                name: Located { value, .. },
                            } => Some(value),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                );
            }
            _ => assert!(false),
        }
    }

    #[test]
    #[cfg(never)]
    /// Broken test
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
        let res = outer.clone().run().unwrap();
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
