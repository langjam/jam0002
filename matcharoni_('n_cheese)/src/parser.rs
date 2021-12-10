use dyn_partial_eq::DynPartialEq;
use itertools::Itertools;
use litrs::{CharLit, StringLit};

pub use parser::program as parse_program;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    PatDef(PatDef),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct PatDef {
    pub name: String,
    pub matches: Vec<Match>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub binding: Binding,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Char(char),
    Concat(Box<Binding>, Box<Binding>),
    ConcatList(Box<Binding>),
    ListOf(Box<Binding>, Option<ListLenBinding>),
    Tuple(Vec<Binding>),
    Named(String, Box<Binding>),
    Shovel(String, Box<Binding>),
    // TODO: ignoring types for now
    Type(String),
    // TODO: should this be an Expr and not only a Ref?
    Ref(String),
    Anything,
}

impl Binding {
    fn from_string(s: &str) -> Result<Self, &'static str> {
        let mut chars = s.chars();
        let (first, rest) = chars
            .next()
            .map(|c| (c, chars.as_str()))
            .ok_or_else(|| "can't bind an empty string")?;
        Ok(if rest.is_empty() {
            Binding::Char(first)
        } else {
            Binding::Concat(
                Box::new(Binding::Char(first)),
                Box::new(Binding::from_string(rest)?),
            )
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ListLenBinding {
    Exact(usize),
    Min(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Name(String),
    ListCompEl(Box<Ref>),
    ListCompIndex(Box<Ref>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Comment(String),
    CharLiteral(char),
    StringLiteral(String),
    IntLiteral(i128),
    TupleLiteral(Vec<Expr>),
    If(Box<Conditional>),
    While(Box<Conditional>),
    Length(Box<Expr>),
    Ref(Ref),
    Block(Vec<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    ListComprehension {
        expr: Box<Expr>,
        over: Box<Expr>,
        binding: Option<Box<Expr>>,
    },
    ListLiteral(Vec<Expr>),
    CallPat(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>, RangeType),
    BinOp(Box<Expr>, Op, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum RangeType {
    Exclusive,
    Inclusive,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Pow,
    Shl,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub cond: Expr,
    pub then: Expr,
    pub r#else: Option<Expr>,
}

peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = statements:(statement_with_whitespace()+) { Program { statements } }

        rule statement_with_whitespace() -> Statement
            = _* statement:statement() _* { statement }
        rule statement() -> Statement
            = pat_def_statement() / expr_statement()


        rule comment_expr() -> Expr
            = comment:comment_string() { Expr::Comment(comment) }
        rule comment_string() -> String
            = "/" "/" onespace()? body:$([^ '\r' | '\n']*)? following:following_comment()*  {
                body.map(|b| b.to_owned()).into_iter().chain(following.into_iter()).join("\n")
            }
        rule following_comment() -> String
            = newline() c:comment_string() {
                if c.starts_with("//") {
                    let c = c.trim_start_matches("//");
                    let c = c.strip_prefix(' ').unwrap_or(c);
                    format!("\n{}", c)
                } else {
                    c
                }
            }

        rule pat_def_statement() -> Statement
            = pat_def:pat_def() { Statement::PatDef(pat_def) }

        rule pat_def() -> PatDef = one_arm_pat_def() / multi_arm_pat_def()

        rule one_arm_pat_def() -> PatDef
            = "pat" _? name:ident() _? r#match:match() { PatDef { name: name.to_owned(), matches: vec![r#match] } }

        rule multi_arm_pat_def() -> PatDef
            = "pat" _? name:ident() _? "{" _ matches:(match() ** comma()) _? "}" {
                PatDef { name: name.to_owned(), matches }
            }

        rule match() -> Match
            = binding:binding() _? "=>" _? expr:expr() { Match { binding, expr } }

        rule binding() -> Binding
            = concat_binding() / scalar_binding()

        #[cache_left_rec]
        rule concat_binding() -> Binding
            = binding1:concat_binding() _? "~" _? binding2:scalar_binding() {
                Binding::Concat(Box::new(binding1), Box::new(binding2))
            } / binding1:scalar_binding() _? "~" _? binding2:scalar_binding() {
                Binding::Concat(Box::new(binding1), Box::new(binding2))
            }

        rule scalar_binding() -> Binding
            = (shovel_binding() / named_binding() / char_binding() / string_binding() /  binding_in_parens() /
               tuple_binding() / concat_list_binding() / list_binding() / any_binding() /
               type_binding() / ref_binding())

        rule tuple_binding() -> Binding
            = "(" _? bindings:(binding() ** comma()) _? ")" {
                Binding::Tuple(bindings)
            }
        rule binding_in_parens() -> Binding
            = "(" _? binding:binding() _? ")" { binding }

        rule concat_list_binding() -> Binding
            = "[" _? binding:binding() _? "..." _? "]" {
                Binding::ConcatList(Box::new(binding))
            }
        rule list_binding() -> Binding
            = "[" _? binding:binding() _? "]" _? len:list_len_binding()? {
                Binding::ListOf(Box::new(binding), len)
            }
        rule list_len_binding() -> ListLenBinding
            = "{" _? inner:list_len_binding_inner() _? "}" { inner }
        rule list_len_binding_inner() -> ListLenBinding
            = min:int() "+" { ListLenBinding::Min(min as _) }
            / exact:int() { ListLenBinding::Exact(exact as _) }

        rule named_binding() -> Binding
            = name:ident() _? "@" _? binding:scalar_binding() {
                Binding::Named(name.to_owned(), Box::new(binding))
            }
        rule shovel_binding() -> Binding
            = name:ident() _? "<@" _? binding:scalar_binding() {
                Binding::Shovel(name.to_owned(), Box::new(binding))
            }
        rule char_binding() -> Binding
            = char:char_lit() { Binding::Char(char) }
        rule string_binding() -> Binding
            = string:string_lit() {? Ok(Binding::from_string(&string)?) }
        rule any_binding() -> Binding
            = "ANY" { Binding::Anything }
        // TODO: actually support type bindings
        rule type_binding() -> Binding
            = name:type_ident() { Binding::Anything }
        rule ref_binding() -> Binding
            = name:ident() { Binding::Ref(name.to_owned()) }

        rule expr_statement() -> Statement = expr:expr() { Statement::Expr(expr) }

        rule expr() -> Expr
            = (comment_expr() / bin_op_expr() / range_expr() / subscript_expr() / assignment_expr() /
               if_else_expr() / if_no_else_expr() / for_with_binding_expr() / for_expr() /
               while_expr() / scalar_expr() / block_expr())

        #[cache_left_rec]
        rule bin_op_expr() -> Expr
            = left:bin_op_expr() _? op:op() _? right:scalar_expr() {
                Expr::BinOp(Box::new(left), op, Box::new(right))
            } / left:scalar_expr() _? op:op() _? right:scalar_expr() {
                Expr::BinOp(Box::new(left), op, Box::new(right))
            }

        rule op() -> Op
            = ("+" { Op::Add } / "/" { Op::Div } / "-" { Op::Sub } / "**" { Op::Pow } /
               "*" { Op::Mul } / "==" { Op::Eq } / "!=" { Op::Neq } / ">=" { Op::Gte } /
               "<<" { Op::Shl } / "<=" { Op::Lte } / ">" { Op::Gt } / "<" { Op::Lt } /
               "&&" { Op::And } / "||" { Op::Or })

        rule range_expr() -> Expr
            = start:scalar_expr() _? ".." _? end:scalar_expr() {
                Expr::Range(Box::new(start), Box::new(end), RangeType::Exclusive)
            } / start:scalar_expr() _? "..=" _? end:scalar_expr() {
                Expr::Range(Box::new(start), Box::new(end), RangeType::Inclusive)
            }

        rule scalar_expr() -> Expr
            = (parens_expr() / tuple_expr() / call_pat_expr() / char_literal_expr() /
               list_comprehension_with_binding_expr() /
               list_comprehension_expr() / list_literal_expr() / string_literal_expr() /
               int_literal_expr() / callable_expr() / length_expr())

        rule callable_expr() -> Expr
            = ref_expr()

        rule parens_expr() -> Expr
            = "(" _? expr:expr() _? ")" { expr }

        rule tuple_expr() -> Expr
            = "(" _? exprs:(expr() ** comma()) _? ")" { Expr::TupleLiteral(exprs) }

        // TODO: can we () call any expr instead of only names?
        #[cache_left_rec]
        rule call_pat_expr() -> Expr
            = pat:(call_pat_expr() / callable_expr()) nbspace()? arg:scalar_expr() {
                Expr::CallPat(Box::new(pat), Box::new(arg))
            }

        rule subscript_expr() -> Expr
            = pat:scalar_expr() nbspace()? "[" _? arg:scalar_expr() _? "]" {
                Expr::CallPat(Box::new(pat), Box::new(arg))
            }

        rule list_comprehension_expr() -> Expr
            = "[" _? expr:expr() _? "<-" _? over:expr() _? "]" {
                Expr::ListComprehension { expr: Box::new(expr), over: Box::new(over), binding: None }
            }

        rule list_comprehension_with_binding_expr() -> Expr
            = "[" _? expr:expr() _? "<-" binding:ref_expr() _? "@" _? over:expr() _? "]" {
                Expr::ListComprehension {
                    expr: Box::new(expr),
                    over: Box::new(over),
                    binding: Some(Box::new(binding))
                }
            }

        rule list_literal_expr() -> Expr
            = "[" _? exprs:(expr() ** comma()) _? "]" { Expr::ListLiteral(exprs) }
        rule assignment_expr() -> Expr
            = lvalue:scalar_expr() _? "=" _? expr:expr() { Expr::Assignment(Box::new(lvalue), Box::new(expr)) }

        rule block_expr() -> Expr
            = "{" _? exprs:(expr() ** whitespace()) _? "}" { Expr::Block(exprs) }

        rule for_expr() -> Expr
            = "for" _? over:scalar_expr() _? expr:block_expr() {
                Expr::ListComprehension { expr: Box::new(expr), over: Box::new(over), binding: None }
            }

        rule for_with_binding_expr() -> Expr
            = "for" _? binding:ref_expr() _? "@" _? over:scalar_expr() _? expr:block_expr() {
                Expr::ListComprehension {
                    expr: Box::new(expr),
                    over: Box::new(over),
                    binding: Some(Box::new(binding)),
                }
            }

        rule while_expr() -> Expr
            = "while" _ cond:scalar_expr() _? then:block_expr() {
                Expr::While(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule if_else_expr() -> Expr
            = "if" _ cond:scalar_expr() _? then:block_expr() _? "else" _? r#else:block_expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: Some(r#else) }))
            }
        rule if_no_else_expr() -> Expr
            = "if" _ cond:scalar_expr() _? then:block_expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule length_expr() -> Expr
            = "#" expr:scalar_expr() { Expr::Length(Box::new(expr)) }
        rule ref_expr() -> Expr
            = r#ref:ref() { Expr::Ref(r#ref) }
        rule char_literal_expr() -> Expr
            = char_lit:char_lit() { Expr::CharLiteral(char_lit) }
        rule string_literal_expr() -> Expr
            = string_lit:string_lit() { Expr::StringLiteral(string_lit) }
        rule int_literal_expr() -> Expr = int:int() { Expr::IntLiteral(int) }

        rule ref() -> Ref
            = name_ref() / list_comp_el_ref() / list_comp_index_ref()

        rule name_ref() -> Ref
            = name:ident() { Ref::Name(name.to_owned()) }
        rule list_comp_el_ref() -> Ref
            = "*" r#ref:ref() { Ref::ListCompEl(Box::new(r#ref)) }
        rule list_comp_index_ref() -> Ref
            = "%" r#ref:ref() { Ref::ListCompIndex(Box::new(r#ref)) }

        rule string_lit() -> String
            = str:$("\"" (!['"'][_] / "\"\"")* "\"") {?
                Ok(StringLit::parse(str).or_else(|e| { dbg!(str, e) ; Err("string_lit: " ) })?.value().to_owned())
            }
        rule char_lit() -> char
            = char:$("'" "\\"? [_] "'") {?
                Ok(CharLit::parse(char).or_else(|e| { dbg!(e) ; Err("char_lit: " ) })?.value())
            }
        rule int() -> i128
            = int:$("0" / "-"? ['1' ..= '9']+ ['0' ..= '9']*) {? int.parse().or(Err("not a number")) }
        rule type_ident() -> &'input str = $(type_ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule type_ident_start() -> &'input str = $(['A'..='Z' | '_']+)
        rule ident() -> &'input str = !("if" / "while" / "for" / "pat") i:$(ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) { i }
        rule ident_start() -> &'input str = $(['a'..='z' | '_']+)
        rule comma() -> () = _? "," _?
        rule nbspace() = onespace()+
        rule onespace() = [' ' | '\t']
        rule newline() = "\n" / "\r\n"
        rule whitespace() = (nbspace() / newline())+
        rule _() = quiet!{ whitespace() }
    }
}
