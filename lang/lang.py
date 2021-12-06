tokens = (
  "META",
  "DATA",
  "CELL",
  "FUNCTIONS",
  "SELECTORS",
  "MATCH",
  "RULES",
  "ALIAS",
  "NAME",
  "LPAREN",
  "RPAREN",
  "LBRACE",
  "RBRACE",
  "INT",
  "BOOL",
  "EQUALS",
  "ASSIGN",
  "ACCESS",
  "LIST",
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
)

t_META = r"Meta"
t_DATA = r"Data"
t_CELL = r"Cell"
t_FUNCTIONS = r"Functions"
t_SELECTORS = r"Selectors"
t_MATCH = r"Match"
t_RULES = r"Rules"
t_ALIAS = r"Alias"
t_NAME = r"[a-zA-Z_][a-zA-Z0-9_]*"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_LBRACE = r"\{"
t_RBRACE = r"\}"
t_INT = r"int"
t_BOOL = r"bool"
t_EQUALS = "=="
t_ASSIGN = "="
t_ACCESS = r"\."
t_LIST = r"\[\]"
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
  lang : meta data
  """

def p_meta(p):
  """
  meta : META LBRACE statements RBRACE
  """

def p_data(p):
  """
  data :
       | DATA LBRACE cell data_statements RBRACE
  """

def p_cell(p):
  """
  cell : CELL LBRACE statements RBRACE
  """

def p_data_statements(p):
  """
  data_statements :
                  | user_cell data_statements
                  | alias data_statements
  """

def p_user_cell(p):
  """
  user_cell : NAME LBRACE statements RBRACE
  """

def p_alias(p):
  """
  alias : ALIAS NAME ASSIGN NAME LBRACE statements RBRACE
  """

def p_statements(p):
  """
  statements : 
             | statement statements
  """

def p_statement(p):
  """
  statement : INT NAME ASSIGN numeric_exp
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
