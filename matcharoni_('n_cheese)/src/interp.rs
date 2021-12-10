use crate::parser;
use crate::parser::{
    Binding, Conditional, Expr, ListLenBinding, Op, PatDef, RangeType, Ref, Statement,
};
use anyhow::bail;
use dyn_clone::DynClone;
use dyn_partial_eq::*;
use gc::{Finalize, Gc, GcCell, Trace};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

mod builtins;
use builtins::print;

#[derive(Debug)]
enum Scope {
    // should values be Rcd / Gcd?
    Block(HashMap<String, Gc<GcCell<Gc<Value>>>>),
    ListComp {
        r#ref: Option<Ref>,
        index: usize,
        list: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
        map: HashMap<String, Gc<GcCell<Gc<Value>>>>,
    },
}

impl Scope {
    fn new_block_scope() -> Self {
        Scope::Block(HashMap::new())
    }

    fn set(&mut self, name: String, value: GcCell<Gc<Value>>) {
        match self {
            Scope::Block(map) | Scope::ListComp { map, .. } => {
                map.insert(name.clone(), Gc::new(value));
            }
        }
    }

    fn has_var(&self, name: &str) -> bool {
        match self {
            Scope::Block(map) | Scope::ListComp { map, .. } => map.contains_key(name),
        }
    }

    fn mut_or_default(&mut self, name: &str) -> Gc<GcCell<Gc<Value>>> {
        match self {
            Scope::Block(map) | Scope::ListComp { map, .. } => {
                let entry = map.entry(name.to_owned());
                entry
                    .or_insert(Gc::new(GcCell::new(Gc::new(Value::Void))))
                    .clone()
            }
        }
    }
}

pub struct Interpreter {
    scope: Vec<Scope>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            scope: vec![Scope::new_block_scope()],
        };
        for (name, pattern) in builtins::builtins() {
            interp.this_scope().unwrap().set(
                name.to_owned(),
                GcCell::new(Gc::new(Value::Pattern(pattern))),
            );
        }
        interp
    }

    fn push_list_comp_scope(
        &mut self,
        r#ref: Option<Ref>,
        list: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
    ) {
        self.scope.push(Scope::ListComp {
            map: HashMap::new(),
            r#ref,
            index: 0,
            list,
        });
    }

    fn incr_list_comp_index(&mut self) -> anyhow::Result<()> {
        let scope = self.scope.last_mut().unwrap();
        match scope {
            Scope::ListComp { index, .. } => Ok(*index += 1),
            _ => bail!("incr_list_comp_index called on non-list-comp scope"),
        }
    }

    fn push_block_scope(&mut self) {
        self.scope.push(Scope::new_block_scope());
    }

    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn this_scope(&mut self) -> anyhow::Result<&mut Scope> {
        self.scope
            .last_mut()
            .ok_or_else(|| anyhow::anyhow!("No scope"))
    }

    pub fn eval_statement(&mut self, statement: &Statement) -> anyhow::Result<()> {
        match statement {
            Statement::PatDef(pat_def) => self.define_pattern(pat_def),
            Statement::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }
        }
    }

    fn define_pattern(&mut self, pat_def: &parser::PatDef) -> anyhow::Result<()> {
        Ok(self.this_scope()?.set(
            pat_def.name.clone(),
            GcCell::new(Gc::new(Value::Pattern(Box::new(pat_def.clone())))),
        ))
    }

    fn eval_expr(&mut self, expr: &Expr) -> anyhow::Result<Gc<Value>> {
        Ok(match expr {
            Expr::Comment(_) => Gc::new(Value::Void),
            Expr::CharLiteral(c) => Gc::new(Value::Char(*c)),
            Expr::StringLiteral(s) => Gc::new(Value::List(Gc::new(GcCell::new(
                s.chars()
                    .map(|c| Gc::new(GcCell::new(Gc::new(Value::Char(c)))))
                    .collect(),
            )))),
            Expr::IntLiteral(i) => Gc::new(Value::Int(*i)),
            Expr::If(cond) => self.eval_if(cond)?,
            Expr::While(cond) => self.eval_while(cond)?,
            Expr::Length(expr) => Gc::new(Value::Int(
                self.eval_expr(expr)?.as_list_cell()?.borrow().len() as _,
            )),
            Expr::Ref(r#ref) => self.eval_ref(r#ref)?,
            Expr::Block(expr) => self.eval_block(expr)?,
            Expr::Assignment(name, expr) => self.eval_assignment(name, expr)?,
            Expr::ListComprehension {
                expr,
                over,
                binding,
            } => self.eval_list_comp(expr, over, binding.as_ref().map(|b| b.as_ref()))?,
            Expr::TupleLiteral(exprs) => Gc::new(Value::Tuple(
                exprs
                    .iter()
                    .map(|e| self.eval_expr(e))
                    .collect::<anyhow::Result<Vec<Gc<Value>>>>()?,
            )),
            Expr::ListLiteral(exprs) => Gc::new(Value::List(Gc::new(GcCell::new(
                exprs
                    .iter()
                    .map(|e| Ok(Gc::new(GcCell::new(self.eval_expr(e)?))))
                    .collect::<anyhow::Result<Vec<Gc<GcCell<Gc<Value>>>>>>()?,
            )))),
            Expr::CallPat(get_pat, arg) => self.eval_call_pat(get_pat, arg)?,
            Expr::Range(low, high, range_type) => self.eval_range(low, high, *range_type)?,
            Expr::BinOp(lhs, op, rhs) => self.eval_bin_op(lhs, *op, rhs)?,
        })
    }

    fn eval_call_pat(&mut self, get_pat: &Expr, arg: &Expr) -> anyhow::Result<Gc<Value>> {
        let pat = self.eval_expr(get_pat)?;
        let arg = self.eval_expr(arg)?;

        // hack for list indices
        let result = if let (Value::List(list), Value::Int(i)) = (&*pat, &*arg) {
            Gc::clone(&(list.borrow()[*i as usize].borrow()))
        } else {
            pat.as_pattern()?.match_full(self, arg)?
        };

        Ok(result)
    }

    fn eval_bin_op(&mut self, lhs: &Expr, op: Op, rhs: &Expr) -> anyhow::Result<Gc<Value>> {
        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;

        if op == Op::Eq {
            return Ok(Gc::new(Value::Int((lhs == rhs).into())));
        }

        let lhs = lhs.as_int()?;
        let rhs = rhs.as_int()?;
        match op {
            Op::Add => Ok(Gc::new(Value::Int(lhs + rhs))),
            Op::Sub => Ok(Gc::new(Value::Int(lhs - rhs))),
            Op::Mul => Ok(Gc::new(Value::Int(lhs * rhs))),
            Op::Div => Ok(Gc::new(Value::Int(lhs / rhs))),
            Op::Neq => Ok(Gc::new(Value::Int((lhs != rhs).into()))),
            Op::Lt => Ok(Gc::new(Value::Int((lhs < rhs).into()))),
            Op::Lte => Ok(Gc::new(Value::Int((lhs <= rhs).into()))),
            Op::Gt => Ok(Gc::new(Value::Int((lhs > rhs).into()))),
            Op::Gte => Ok(Gc::new(Value::Int((lhs >= rhs).into()))),
            Op::And => Ok(Gc::new(Value::Int((lhs != 0 && rhs != 0).into()))),
            Op::Or => Ok(Gc::new(Value::Int((lhs != 0 || rhs != 0).into()))),
            Op::Pow => Ok(Gc::new(Value::Int(lhs.pow(rhs as _)))),
            Op::Shl => Ok(Gc::new(Value::Int(lhs << rhs))),
            Op::Eq => unreachable!(),
        }
    }

    fn eval_range(
        &mut self,
        low: &Expr,
        high: &Expr,
        range_type: RangeType,
    ) -> anyhow::Result<Gc<Value>> {
        let lhs = self.eval_expr(low)?.as_int()?;
        let rhs = self.eval_expr(high)?.as_int()?;
        let (low, high) = if lhs < rhs { (lhs, rhs) } else { (rhs, lhs) };
        Ok(Gc::new(Value::List(Gc::new(GcCell::new(
            match range_type {
                RangeType::Exclusive => (low..high)
                    .map(|i| Gc::new(GcCell::new(Gc::new(Value::Int(i)))))
                    .collect(),
                RangeType::Inclusive => (low..=high)
                    .map(|i| Gc::new(GcCell::new(Gc::new(Value::Int(i)))))
                    .collect(),
            },
        )))))
    }

    fn eval_block(&mut self, block: &[Expr]) -> anyhow::Result<Gc<Value>> {
        let mut res = Gc::new(Value::Void);
        self.push_block_scope();
        for expr in block {
            res = self.eval_expr(expr)?;
        }
        self.pop_scope();
        Ok(res)
    }

    fn eval_if(
        &mut self,
        Conditional { cond, then, r#else }: &Conditional,
    ) -> anyhow::Result<Gc<Value>> {
        let cond_val = self.eval_expr(cond)?;
        if cond_val.is_true()? {
            let ret = self.eval_expr(then)?;
            if let Some(_) = r#else {
                Ok(ret)
            } else {
                Ok(Gc::new(Value::Void))
            }
        } else if let Some(r#else) = r#else {
            self.eval_expr(r#else)
        } else {
            Ok(Gc::new(Value::Void))
        }
    }

    fn eval_while(
        &mut self,
        Conditional { cond, then, .. }: &Conditional,
    ) -> anyhow::Result<Gc<Value>> {
        let mut cond_val = self.eval_expr(cond)?;
        while cond_val.is_true()? {
            self.eval_expr(then)?;
            cond_val = self.eval_expr(cond)?;
        }
        Ok(Gc::new(Value::Void))
    }

    fn eval_ref(&mut self, r#ref: &Ref) -> anyhow::Result<Gc<Value>> {
        Ok(match r#ref {
            Ref::Name(var_name) => self.eval_ref_name(var_name)?,
            Ref::ListCompEl(r#ref) => self.eval_ref_list_comp_el(r#ref)?,
            Ref::ListCompIndex(r#ref) => self.eval_list_comp_index(r#ref)?,
        })
    }

    fn eval_ref_name(&mut self, var_name: &str) -> anyhow::Result<Gc<Value>> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::Block(scope) => scope.get(var_name).map(|v| v.borrow().clone()),
                Scope::ListComp {
                    map: _,
                    r#ref: Some(Ref::Name(name)),
                    index: _,
                    list,
                } if name == var_name => Some(Gc::new(Value::List(list.clone()))),
                Scope::ListComp { map, .. } => map.get(var_name).map(|v| v.borrow().clone()),
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {} not found", var_name))
    }

    fn eval_ref_list_comp_el(&mut self, search_ref: &Ref) -> anyhow::Result<Gc<Value>> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::ListComp {
                    map: _,
                    r#ref: Some(r#ref),
                    index,
                    list,
                } if r#ref == search_ref => list.borrow().get(*index).map(|v| v.borrow().clone()),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {:?} not found", search_ref))
    }

    fn eval_list_comp_index(&mut self, search_ref: &Ref) -> anyhow::Result<Gc<Value>> {
        // TODO: duped with eval_list_comp_el
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::ListComp {
                    map: _,
                    r#ref: Some(r#ref),
                    index,
                    list: _,
                } if r#ref == search_ref => Some(Gc::new(Value::Int(*index as _))),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {:?} not found", search_ref))
    }

    fn eval_list_comp(
        &mut self,
        expr: &Expr,
        over: &Expr,
        binding: Option<&Expr>,
    ) -> anyhow::Result<Gc<Value>> {
        let r#ref = match (over, binding) {
            (Expr::Ref(_), Some(Expr::Ref(_))) => {
                bail!("can't currently double bind list comprehension")
            }
            (Expr::Ref(r#ref), None) | (_, Some(Expr::Ref(r#ref))) => Some(r#ref.clone()),
            _ => None,
        };
        let val = self.eval_expr(over)?;
        let list = val.as_list_cell()?;
        let len = list.borrow().len();
        let mut ret = Vec::with_capacity(len);
        self.push_list_comp_scope(r#ref, val.as_list_cell()?);
        for _ in 0..len {
            ret.push(Gc::new(GcCell::new(self.eval_expr(expr)?)));
            self.incr_list_comp_index()?;
        }
        self.pop_scope();
        Ok(Gc::new(Value::List(Gc::new(GcCell::new(ret)))))
    }

    fn eval_assignment(&mut self, lvalue: &Expr, expr: &Expr) -> anyhow::Result<Gc<Value>> {
        let result = self.eval_expr(expr)?;
        self.assign_into(lvalue, result)?;
        Ok(Gc::new(Value::Void))
    }

    fn assign_into(&mut self, lvalue: &Expr, val: Gc<Value>) -> anyhow::Result<()> {
        if let Expr::TupleLiteral(exprs) = lvalue {
            if let Value::Tuple(results) = &*val {
                if exprs.len() != results.len() {
                    bail!("Tuple length mismatch {:?} {:?}", exprs, results.len());
                }
                for (inner_lvalue, inner_value) in exprs.iter().zip(results.into_iter()) {
                    self.assign_into(inner_lvalue, inner_value.clone())?;
                }
            }
        } else {
            *(self.eval_lvalue(lvalue)?.borrow_mut()) = val;
        }
        Ok(())
    }

    fn eval_lvalue(&mut self, expr: &Expr) -> anyhow::Result<Gc<GcCell<Gc<Value>>>> {
        Ok(match expr {
            Expr::Ref(Ref::Name(var_name)) => {
                let existing_i = self
                    .scope
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(i, scope)| scope.has_var(var_name).then(|| i));
                if let Some(i) = existing_i {
                    &mut self.scope[i]
                } else {
                    self.this_scope()?
                }
                .mut_or_default(var_name)
            }
            Expr::Ref(Ref::ListCompEl(box search_ref)) => {
                // // TODO: duped from eval_ref_list_comp_el
                self.scope
                    .iter()
                    .rev()
                    .find_map(|scope| match scope {
                        Scope::ListComp {
                            map: _,
                            r#ref: Some(r#ref),
                            index,
                            list,
                        } if r#ref == search_ref => {
                            list.borrow_mut().get(*index).map(|v| v.clone())
                        }
                        _ => None,
                    })
                    .ok_or_else(|| anyhow::anyhow!("Variable {:?} not found", search_ref))?
            }
            Expr::CallPat(get_pat, arg) => {
                let i = self.eval_expr(arg)?.as_int()?;
                let pat = self.eval_lvalue(get_pat)?;
                let pat = pat.borrow();
                pat.as_list_cell()
                    .map(|l| l.borrow_mut()[i as usize].clone())?
            }
            Expr::IntLiteral(_)
            | Expr::Ref(Ref::ListCompIndex(_))
            | Expr::TupleLiteral(_)
            | Expr::If(_)
            | Expr::While(_)
            | Expr::Length(_)
            | Expr::Block(_)
            | Expr::Assignment(_, _)
            | Expr::ListComprehension { .. }
            | Expr::ListLiteral(_)
            | Expr::Range(_, _, _)
            | Expr::BinOp(_, _, _)
            | Expr::Comment(_)
            | Expr::CharLiteral(_)
            | Expr::StringLiteral(_) => return Err(anyhow::anyhow!("Invalid lvalue: {:?}", expr)),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Trace, Finalize)]
pub enum Value {
    Void,
    Char(char),
    Tuple(Vec<Gc<Value>>),
    List(Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>),
    Int(i128),
    Pattern(#[unsafe_ignore_trace] Box<dyn Pattern>),
}

#[dyn_partial_eq]
pub trait Pattern: Debug + DynClone {
    fn name(&self) -> &str;
    fn match_full(&self, interp: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>>;
    fn match_partial(
        &self,
        interp: &mut Interpreter,
        arg: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
    ) -> anyhow::Result<Option<(Gc<Value>, Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>)>> {
        self.match_full(interp, Gc::new(Value::List(arg)))
            .map(|val| Some((val, Gc::new(GcCell::new(vec![])))))
    }
}

dyn_clone::clone_trait_object!(Pattern);

impl Pattern for parser::PatDef {
    fn name(&self) -> &str {
        &self.name
    }

    fn match_full(&self, interp: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>> {
        let matched_pattern = match_pat_def_full(interp, self, arg.clone())?.ok_or_else(|| {
            anyhow::anyhow!("Pattern {:?} not matched on {}", self.name(), print(&arg))
        })?;
        interp.push_block_scope();
        for (name, val) in matched_pattern.bindings {
            interp.this_scope()?.set(name, GcCell::new(val));
        }
        let res = interp.eval_expr(&matched_pattern.expr)?;
        interp.pop_scope();
        Ok(res)
    }

    fn match_partial(
        &self,
        interp: &mut Interpreter,
        arg: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
    ) -> anyhow::Result<Option<(Gc<Value>, Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>)>> {
        let partial_match = match_pat_def_partial(interp, self, arg.clone())?;
        if partial_match.is_none() {
            return Ok(None);
        }
        let (matched_pattern, rest) = partial_match.unwrap();
        interp.push_block_scope();
        for (name, val) in matched_pattern.bindings {
            interp.this_scope()?.set(name, GcCell::new(val));
        }
        let res = Some((interp.eval_expr(&matched_pattern.expr)?, rest));
        interp.pop_scope();
        Ok(res)
    }
}

#[derive(Debug)]
struct MatchedPattern {
    bindings: Vec<(String, Gc<Value>)>,
    expr: Expr,
}

fn match_pat_def_full(
    interp: &mut Interpreter,
    pat_def: &PatDef,
    val: Gc<Value>,
) -> anyhow::Result<Option<MatchedPattern>> {
    // TODO?: perhaps it would be simpler to use an Or pattern here instead of having match arms be a
    // special case
    for match_arm in &pat_def.matches {
        if let Some(matched) = match_full(interp, val.clone(), &match_arm.binding)? {
            let bindings = matched.collect_named_matches();
            return Ok(Some(MatchedPattern {
                bindings,
                expr: match_arm.expr.clone(),
            }));
        }
    }
    Ok(None)
}

fn match_pat_def_partial(
    interp: &mut Interpreter,
    pat_def: &PatDef,
    vals: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
) -> anyhow::Result<Option<(MatchedPattern, Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>)>> {
    for match_arm in &pat_def.matches {
        if let Some((matched, rest)) = match_partial(interp, vals.clone(), &match_arm.binding)? {
            // TODO: duped with match_pat_def_full
            let bindings = matched.collect_named_matches();
            return Ok(Some((
                MatchedPattern {
                    bindings,
                    expr: match_arm.expr.clone(),
                },
                rest,
            )));
        }
    }
    Ok(None)
}

#[derive(Debug, Clone)]
enum MatchName {
    Scalar(String),
    Shovel(String),
}

#[derive(Debug, Clone)]
struct Match {
    value: Gc<Value>,
    name: Option<MatchName>,
    inner_matches: Vec<Match>,
}

impl Match {
    fn unnamed(value: Gc<Value>) -> Self {
        Self {
            value,
            name: None,
            inner_matches: vec![],
        }
    }

    fn with_inner_matches(mut self, inner_matches: Vec<Match>) -> Self {
        self.inner_matches = inner_matches;
        self
    }

    fn add_name(mut self, name: MatchName) -> Self {
        // if this match is already bound to another name, then keep the name, adding the new name
        if self.has_name() {
            self.inner_matches.push(self.clone());
        }
        self.name = Some(name);
        self
    }

    fn has_name(&self) -> bool {
        self.name.is_some()
    }

    // TODO: return Result instead of panicking
    fn collect_named_matches(&self) -> Vec<(String, Gc<Value>)> {
        let mut shovels: HashMap<String, usize> = HashMap::new();
        let mut scalars = HashSet::new();

        let mut matches = vec![];
        for (match_name, value) in self
            .all_matches()
            .into_iter()
            .filter_map(|m| Some((m.name?, m.value)))
        {
            match match_name {
                MatchName::Scalar(name) => {
                    if scalars.contains(&name) {
                        panic!("Duplicate scalar match name: {}", name);
                    }
                    matches.push((name.to_owned(), value.clone()));
                    scalars.insert(name);
                }
                MatchName::Shovel(name) => {
                    if let Some(&index) = shovels.get(&name) {
                        let list = matches[index].1.as_list_cell().unwrap();
                        list.borrow_mut().push(Gc::new(GcCell::new(value)));
                    } else {
                        shovels.insert(name.to_owned(), matches.len());
                        matches.push((
                            name,
                            Gc::new(Value::List(Gc::new(GcCell::new(vec![Gc::new(
                                GcCell::new(value.clone()),
                            )])))),
                        ));
                    }
                }
            }
        }
        matches
    }

    fn all_matches(&self) -> Vec<Match> {
        let mut matches = vec![self.clone()];
        for inner_match in &self.inner_matches {
            matches.extend(inner_match.all_matches());
        }
        matches
    }
}

fn match_full(
    interp: &mut Interpreter,
    val: Gc<Value>,
    binding: &parser::Binding,
) -> anyhow::Result<Option<Match>> {
    Ok(match binding {
        Binding::Char(c) => {
            if Value::Char(*c) == *val {
                Some(Match::unnamed(val))
            } else {
                None
            }
        }
        Binding::ListOf(_, _) | Binding::ConcatList(_) | Binding::Concat(_, _) => {
            if let Value::List(vals) = &*val {
                if let Some((matched, rest)) = match_partial(interp, vals.clone(), binding)? {
                    if rest.borrow().is_empty() {
                        Some(matched)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Binding::Tuple(bindings) => {
            if let Value::Tuple(vals) = &*val {
                let matches = vals
                    .iter()
                    .zip(bindings.iter())
                    .map(|(val, binding)| match_full(interp, val.clone(), binding))
                    .collect::<anyhow::Result<Vec<Option<Match>>>>()?;
                if matches.iter().all(Option::is_some) {
                    let inner_matches = matches.into_iter().flatten().collect();
                    Some(Match::unnamed(val).with_inner_matches(inner_matches))
                } else {
                    None
                }
            } else {
                None
            }
        }
        Binding::Shovel(name, binding) => match_full(interp, val, binding)?
            .map(|m| m.add_name(MatchName::Shovel(name.to_owned()))),
        Binding::Named(name, binding) => match_full(interp, val, binding)?
            .map(|m| m.add_name(MatchName::Scalar(name.to_owned()))),
        Binding::Anything => Some(Match::unnamed(val)),
        Binding::Type(_) => unreachable!("types are unimplemented"),
        // TODO: this can be an expr instead of a ref
        Binding::Ref(name) => {
            // TODO: this function needs to return a Result instead of panicking in here
            let pat = interp.eval_ref_name(&name);
            let pat = pat.unwrap_or_else(|_| panic!("ref {} not found", name));
            let pat = pat
                .as_pattern()
                .unwrap_or_else(|_| panic!("ref {} is not a pattern", name));

            let evalled = pat.match_full(interp, val);
            match evalled {
                Err(err) => {
                    println!("warning: {}", err);
                    None
                }
                Ok(val) => Some(Match::unnamed(val)),
            }
        }
    })
}

// TODO: maybe this could work with iterators?
// returns the remaining unmatched list if any
fn match_partial(
    interp: &mut Interpreter,
    vals: Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>,
    binding: &parser::Binding,
) -> anyhow::Result<Option<(Match, Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>)>> {
    Ok(match binding {
        Binding::Anything | Binding::Type(_) | Binding::Char(_) | Binding::Tuple(_) => {
            let vals = vals.borrow();
            if vals.is_empty() {
                return Ok(None);
            }
            let val = Gc::clone(&vals.first().unwrap().borrow());
            if let Some(matched) = match_full(interp, val, binding)? {
                Some((
                    matched,
                    Gc::new(GcCell::new(vals.iter().skip(1).cloned().collect())),
                ))
            } else {
                None
            }
        }
        Binding::ConcatList(_) => {
            todo!()
            // let mut matched = vec![];
            // for val in vals {
            //     if let Some(m) = match_binding(interp, val, binding) {
            //         matched.push(m);
            //     } else {
            //         return None;
            //     }
            // }
        }
        Binding::Concat(left, right) => {
            let left = match_partial(interp, vals.clone(), left)?;
            if left.is_none() {
                return Ok(None);
            }
            let (left_matched, rest) = left.unwrap();

            let right = match_partial(interp, rest, right)?;
            if right.is_none() {
                return Ok(None);
            }
            let (right_matched, rest) = right.unwrap();
            let this_match = Match::unnamed(Gc::new(Value::List(vals)))
                .with_inner_matches(vec![left_matched, right_matched]);
            Some((this_match, rest))
        }
        Binding::ListOf(binding, len_binding) => {
            let mut matched = vec![];
            let mut inners = vec![];
            let mut rest = vals.clone();
            match len_binding {
                Some(ListLenBinding::Min(size)) | Some(ListLenBinding::Exact(size)) => {
                    for _ in 0..*size {
                        let inner = match_partial(interp, rest, binding)?;
                        if inner.is_none() {
                            return Ok(None);
                        }
                        let (inner_match, inner_rest) = inner.unwrap();
                        inners.push(inner_match.clone());
                        matched.push(Gc::new(GcCell::new(inner_match.value)));
                        rest = inner_rest;
                    }
                }
                None => {}
            }
            if let Some(ListLenBinding::Exact(_)) = len_binding {
                return Ok(Some((
                    Match::unnamed(Gc::new(Value::List(Gc::new(GcCell::new(matched)))))
                        .with_inner_matches(inners),
                    rest,
                )));
            }
            while let Some((inner_match, inner_rest)) =
                match_partial(interp, rest.clone(), binding)?
            {
                inners.push(inner_match.clone());
                matched.push(Gc::new(GcCell::new(inner_match.value)));
                rest = inner_rest;

                if rest.borrow().is_empty() {
                    break;
                }
            }
            Some((
                Match::unnamed(Gc::new(Value::List(Gc::new(GcCell::new(matched)))))
                    .with_inner_matches(inners),
                rest,
            ))
        }
        Binding::Shovel(name, binding) => match_partial(interp, vals, binding)?
            .map(|(m, rest)| (m.add_name(MatchName::Shovel(name.to_owned())), rest)),
        Binding::Named(name, binding) => match_partial(interp, vals, binding)?
            .map(|(m, rest)| (m.add_name(MatchName::Scalar(name.to_owned())), rest)),
        // TODO: Binding::Ref could just be Binding::Expr
        Binding::Ref(name) => {
            let pat = interp.eval_ref_name(name)?;
            let pat = pat.as_pattern()?;
            let matched = pat.match_partial(interp, vals)?;
            if matched.is_none() {
                return Ok(None);
            }
            let (val, rest) = matched.unwrap();
            Some((Match::unnamed(val), rest))
        }
    })
}

impl Value {
    fn collect_string(&self) -> anyhow::Result<String> {
        Ok(self
            .as_list_cell()?
            .borrow()
            .iter()
            .map(|v| v.borrow().as_char())
            .collect::<anyhow::Result<String>>()?)
    }

    fn as_pattern(&self) -> anyhow::Result<&dyn Pattern> {
        match self {
            Value::Pattern(p) => Ok(p.as_ref()),
            _ => Err(anyhow::anyhow!("{:?} is not a pattern", self)),
        }
    }

    fn as_char(&self) -> anyhow::Result<char> {
        match self {
            Value::Char(c) => Ok(*c),
            _ => Err(anyhow::anyhow!("not a char")),
        }
    }

    fn as_int(&self) -> anyhow::Result<i128> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(anyhow::anyhow!("{:?} is not an int", self)),
        }
    }

    fn as_list_cell(&self) -> anyhow::Result<Gc<GcCell<Vec<Gc<GcCell<Gc<Value>>>>>>> {
        match self {
            Value::List(l) => Ok(l.clone()),
            _ => Err(anyhow::anyhow!("{:?} is not a list", self)),
        }
    }

    fn as_tuple(&self) -> anyhow::Result<&Vec<Gc<Value>>> {
        match self {
            Value::Tuple(l) => Ok(l),
            _ => Err(anyhow::anyhow!("not a tuple")),
        }
    }

    fn is_true(&self) -> anyhow::Result<bool> {
        Ok(self.as_int()? != 0)
    }
}
