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

def t_META(t):
  r"Meta"
  return t

def t_CELL(t):
  r"Cell"
  return t

def t_SELECTORS(t):
  r"Selectors"
  return t

def t_MATCHCOUNT(t):
  r"MatchCount"
  return t

def t_RULES(t):
  r"Rules"
  return t

def t_ALIASES(t):
  r"Aliases"
  return t

def t_LPAREN(t):
  r"\("
  return t

def t_RPAREN(t):
  r"\)"
  return t

def t_LBRACE(t):
  r"\{"
  return t

def t_RBRACE(t):
  r"\}"
  return t

def t_LSQBRACE(t):
  r"\["
  return t

def t_RSQBRACE(t):
  r"\]"
  return t

def t_INT(t):
  r"int"
  return t

def t_BOOL(t):
  r"bool"
  return t

def t_EQUALS(t):
  "=="
  return t

def t_ASSIGN(t):
  "="
  return t

def t_NOT(t):
  r"\!"
  return t

def t_PLUS(t):
  r"\+"
  return t

def t_MINUS(t):
  r"\-"
  return t

def t_MULTIPLY(t):
  r"\*"
  return t

def t_DIVIDE(t):
  r"\/"
  return t

def t_AND(t):
  r"\&"
  return t

def t_OR(t):
  r"\|"
  return t

def t_GREATER(t):
  r"\>"
  return t

def t_LESS(t):
  r"\<"
  return t

def t_GREATEREQ(t):
  r"\>="
  return t

def t_LESSEQ(t):
  r"\<="
  return t

def t_NW(t):
  r"NW"
  return t

def t_N(t):
  r"N"
  return t

def t_NE(t):
  r"NE"
  return t

def t_W(t):
  r"W"
  return t

def t_E(t):
  r"E"
  return t

def t_SW(t):
  r"SW"
  return t

def t_S(t):
  r"S"
  return t

def t_SE(t):
  r"SE"
  return t

def t_NEIGHBOURS(t):
  r"neighbours"
  return t

def t_ROW(t):
  r"row"
  return t

def t_COL(t):
  r"col"
  return t

def t_COMMA(t):
  r","
  return t

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

def t_NAME(t):
  r"[a-zA-Z_][a-zA-Z0-9_]*"
  return t

def t_newline(t):
  r"\n+"
  t.lexer.lineno += t.value.count("\n")

t_ignore = " \t"

def t_error(t):
  print(f"Illegal character {t.value[0]!r}")
  t.lexer.skip(1)


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

def p_declarations(p):
  """
  declarations : 
               | declaration declarations
  """

def p_declaration(p):
  """
  declaration : INT NAME ASSIGN expression
              | BOOL NAME ASSIGN expression
  """

def p_expression(p):
  """
  expression : expression PLUS expression
             | expression MINUS expression
             | expression MULTIPLY expression
             | expression DIVIDE expression
             | MINUS expression %prec UMINUS
             | LPAREN expression RPAREN
             | NUMBER
             | ROW
             | COL
             | MATCHCOUNT LPAREN list COMMA NAME RPAREN

             | expression GREATER expression
             | expression LESS expression
             | expression GREATEREQ expression
             | expression LESSEQ expression
             | expression EQUALS expression
             | expression OR expression
             | expression AND expression
             | NOT expression
             | TRUE
             | FALSE

             | NAME
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

def p_error(p):
  print(f"Syntax error at {p.value!r}: {repr(p)}")

import ply.yacc as yacc
yacc.yacc()

with open("./tests/test.cel", "r") as f:
  code = f.read()

yacc.parse(code)
