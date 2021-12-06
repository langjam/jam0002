tokens = (
  "META",
  "CELL",
  "SELECTORS",
  "MATCHCOUNT",
  "RULES",
  "ALIASES",
  "LPAREN",
  "RPAREN",
  "LBRACE",
  "RBRACE",
  "LSQBRACE",
  "RSQBRACE",
  "INT",
  "BOOL",
  "EQUALS",
  "ASSIGN",
  "NOT",
  "PLUS",
  "MINUS",
  "MULTIPLY",
  "DIVIDE",
  "AND",
  "OR",
  "GREATER",
  "LESS",
  "GREATEREQ",
  "LESSEQ",
  "NUMBER",
  "TRUE",
  "FALSE",
  "NW", "N", "NE",
  "W", "E",
  "SW", "S", "SE",
  "NEIGHBOURS",
  "ROW",
  "COL",
  "COMMA",
  "NAME",
)

t_META = r"Meta"
t_CELL = r"Cell"
t_SELECTORS = r"Selectors"
t_MATCHCOUNT = r"MatchCount"
t_RULES = r"Rules"
t_ALIASES = r"Aliases"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_LBRACE = r"\{"
t_RBRACE = r"\}"
t_LSQBRACE = r"\["
t_RSQBRACE = r"\]"
t_INT = r"int"
t_BOOL = r"bool"
t_EQUALS = "=="
t_ASSIGN = "="
t_NOT = r"!"
t_PLUS = r"\+"
t_MINUS = r"\-"
t_MULTIPLY = r"\*"
t_DIVIDE = r"\/"
t_AND = r"\&"
t_OR = r"\|"
t_GREATER = r"\>"
t_LESS = r"\<"
t_GREATEREQ = r"\>="
t_LESSEQ = r"\<="
t_NW = r"NW"
t_N = r"N"
t_NE = r"NE"
t_W = r"W"
t_E = r"E"
t_SW = r"SW"
t_S = r"S"
t_SE = r"SE"
t_NEIGHBOURS = r"neighbours"
t_ROW = r"row"
t_COL = r"col"
t_COMMA = r","

def t_NUMBER(t):
  r"[0-9]+"
  t.value = int(t.value)
  return t

def t_TRUE(t):
  r"true"
  t.value = True
  return t

def t_FALSE(t):
  r"false"
  t.value = False
  return t

t_NAME = r"[a-zA-Z_][a-zA-Z0-9_]*"

t_ignore = " \t"

def t_newline(t):
  r"\n+"
  t.lexer.lineno += t.value.count("\n")

def t_error(t):
  print(f"Illegal character {t.value[0]!r}")
  t.lexer.skip (1)

import ply.lex as lex
lex.lex()

precedence = (
  ("left", "PLUS", "MINUS"),
  ("left", "MULTIPLY", "DIVIDE"),
  ("left", "UMINUS"),
  ("left", "EQUALS", "GREATER", "LESS", "GREATEREQ", "LESSEQ"),
  ("left", "OR"),
  ("left", "AND"),
  ("left", "NOT"),
)

def p_lang(p):
  """
  lang : meta cell aliases selectors rules
  """

def p_meta(p):
  """
  meta : META LBRACE statements RBRACE
  """

def p_cell(p):
  """
  cell : CELL LBRACE declarations RBRACE
  """

def p_aliases(p):
  """
  aliases :
          | ALIASES LBRACE statement_groups RBRACE
  """

def p_selectors(p):
  """
  selectors :
            | SELECTORS LBRACE statements RBRACE
  """

def p_rules(p):
  """
  rules :
        | RULES LBRACE rule_statements RBRACE
  """

def p_rule_statements(p):
  """
  rule_statements :
                  | rule_statement rule_statements
  """

def p_rule_statement(p):
  """
  rule_statement : NAME LPAREN NAME RPAREN ASSIGN LBRACE statements RBRACE
  """

def p_statement_groups(p):
  """
  statement_groups :
                   | statement_group statement_groups
  """

def p_statement_group(p):
  """
  statement_group : NAME ASSIGN LBRACE statements RBRACE
  """

def p_statements(p):
  """
  statements :
             | statement statements
  """

def p_statement(p):
  """
  statement : NAME ASSIGN expression
  """

def p_expression(p):
  """
  expression : numeric_exp
             | bool_exp
  """

def p_declarations(p):
  """
  declarations : 
               | declaration declarations
  """

def p_declaration(p):
  """
  declaration : INT NAME ASSIGN numeric_exp
              | BOOL NAME ASSIGN bool_exp
  """

def p_numeric_exp(p):
  """
  numeric_exp : numeric_exp PLUS numeric_exp
              | numeric_exp MINUS numeric_exp
              | numeric_exp MULTIPLY numeric_exp
              | numeric_exp DIVIDE numeric_exp
              | MINUS numeric_exp %prec UMINUS
              | LPAREN numeric_exp RPAREN
              | NUMBER
              | NAME
              | ROW
              | COL
              | MATCHCOUNT LPAREN list COMMA NAME RPAREN
  """

def p_list(p):
  """
  list : LSQBRACE directions RSQBRACE
       | NEIGHBOURS
  """

def p_directions(p):
  """
  directions : NW
             | N
             | NE
             | W
             | E
             | SW
             | S 
             | SE 
  """

def p_bool_exp(p):
  """
  bool_exp : numeric_exp GREATER numeric_exp
           | numeric_exp LESS numeric_exp
           | numeric_exp GREATEREQ numeric_exp
           | numeric_exp LESSEQ numeric_exp
           | bool_exp EQUALS bool_exp
           | bool_exp OR bool_exp
           | bool_exp AND bool_exp
           | NOT bool_exp
           | LPAREN bool_exp RPAREN
           | TRUE
           | FALSE
  """

def p_error(p):
  print(f"Syntax error at {p.value!r}")

import ply.yacc as yacc
yacc.yacc()
