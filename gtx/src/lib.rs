pub mod ast;
pub mod parser;

use std::{collections::HashMap, fmt::Display};

use parser::Spanned;

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
    Match { on: Box<Ast>, clauses: Vec<Clause> },
    Constr { name: String, args: Vec<Ast> },
}

struct DisplayAst<'ctx, 'ast> {
    offset: usize,
    context: &'ctx AstContext,
    ast: &'ast Ast,
}

impl<'ctx, 'ast> Display for DisplayAst<'ctx, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ast {
            Ast::Var { name } => match self.context.variable(*name) {
                Some(name) => write!(f, "{}", name),
                None => write!(f, "x{}", name),
            },
            Ast::Match { on, clauses } => {
                write!(f, "case {} of\n", on.display(self.context))?;
                for cl in clauses {
                    write!(
                        f,
                        "{:width$}| {} {} {}\n",
                        "",
                        cl.pattern,
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
            Ast::Constr { name, args } => {
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
    pattern: Pat,
    body: Ast,
}

impl Clause {
    fn run(&self, matched: Ast) -> Option<Ast> {
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

    pub fn run(&self, ctx: &AstContext) -> Option<Self> {
        match self {
            Ast::Var { name } => {
                let var_ast = ctx.binding(ctx.variable(*name)?)?;
                var_ast.run(ctx)
            }
            Ast::Match { on, clauses } => {
                let arg = on.run(ctx)?;
                let (rec, res) = clauses
                    .into_iter()
                    .find_map(|cl| Some((cl.recursive, cl.run(arg.clone())?)))?;
                if rec {
                    Ast::Match {
                        on: Box::new(res),
                        clauses: clauses.clone(),
                    }
                    .run(ctx)
                } else {
                    Some(res)
                }
            }
            Ast::Constr { name, args } => {
                let args = args.iter().map(|ast| ast.run(ctx)).collect::<Option<_>>()?;
                Some(Ast::Constr {
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

#[derive(Debug, Default)]
pub struct AstContext {
    var_data: HashMap<String, Ast>,
    var_ref: HashMap<u32, String>,
    next_free: u32,
}

impl AstContext {
    pub fn binding<S: AsRef<str>>(&self, name: S) -> Option<&Ast> {
        self.var_data.get(name.as_ref())
    }

    pub fn variable(&self, var: u32) -> Option<&str> {
        self.var_ref.get(&var).map(|s| s.as_str())
    }

    pub fn add_decl(&mut self, decl: ast::Decl) {
        let ast = self.make_expr(decl.body.into_inner());
        self.var_data.insert(decl.name.into_inner(), ast);
    }

    pub fn make_expr(&mut self, ast: ast::Expr) -> Ast {
        match ast {
            ast::Expr::Primary(p) => self.make_prim_expr(p),
            ast::Expr::Binop { .. } => todo!(),
            ast::Expr::Match {
                on: Spanned(_, on),
                arms,
            } => Ast::Match {
                on: Box::new(self.make_expr((*on).clone())),
                clauses: arms
                    .into_iter()
                    .map(|clause| self.make_clause(clause))
                    .collect(),
            },
        }
    }

    pub fn make_clause(&mut self, clause: ast::Clause) -> Clause {
        Clause {
            recursive: clause.recursive,
            body: self.make_expr(clause.body.into_inner()),
            pattern: self.make_pattern(clause.pattern.into_inner()),
        }
    }

    pub fn make_pattern(&mut self, pattern: ast::Pattern) -> Pat {
        match pattern {
            ast::Pattern::Primary(p) => self.make_prim_pattern(p),
            ast::Pattern::Constructor {
                name: Spanned(_, name),
                args,
            } => Pat::PatConstr {
                name,
                args: args
                    .into_iter()
                    .map(|Spanned(_, pat)| self.make_pattern(pat))
                    .collect(),
            },
        }
    }

    pub fn make_prim_expr(&mut self, prim: ast::Primary) -> Ast {
        match prim {
            ast::Primary::Const(c) => Ast::Constr {
                name: c,
                args: vec![],
            },
            ast::Primary::Var(v) => Ast::Var {
                name: self.next_var(v),
            },
        }
    }

    pub fn make_prim_pattern(&mut self, prim: ast::Primary) -> Pat {
        match prim {
            ast::Primary::Const(c) => Pat::PatConstr {
                name: c,
                args: vec![],
            },
            ast::Primary::Var(v) => Pat::PatVar {
                name: self.next_var(v),
            },
        }
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
    use crate::parser::ReplParse;
    use crate::Ast;
    use crate::AstContext;

    use super::Ast::*;

    fn parse_expr(
        expr: &str,
    ) -> Result<(AstContext, Ast), codespan_reporting::diagnostic::Diagnostic<codespan::FileId>>
    {
        let mut files = codespan::Files::new();
        let file_id = files.add("<test>", expr);
        let ast = crate::parser::parse_repl(&files, file_id)?;
        if let crate::parser::Spanned(_, ReplParse::Expr(expr)) = ast {
            let mut ctx = AstContext::default();
            let expr = ctx.make_expr(expr);
            Ok((ctx, expr))
        } else {
            unreachable!()
        }
    }

    #[test]
    pub fn test1() {
        let (ctx, prog) = parse_expr("case x1 of | x match x end").unwrap();
        let target = Var { name: 1 };
        let res = prog.run(&ctx);
        println!("{:?}", res);
        assert_eq!(res, Some(target))
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
        let target = Var { name: 1 };
        let res = prog.run(&ctx);
        println!("{:?}", res);
        assert_eq!(res, Some(target))
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
        assert_eq!(res, None)
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
        let target = Constr {
            name: "First2".to_string(),
            args: vec![Var { name: 1 }, Var { name: 2 }],
        };
        let res = prog.run(&ctx);
        println!("{:?}", res);
        assert_eq!(res, Some(target))
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
