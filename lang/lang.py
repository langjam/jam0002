reserved = {
  "Meta": "META",
  "Cell": "CELL",
  "Selectors": "SELECTORS",
  "MatchCount": "MATCHCOUNT",
  "Rules": "RULES",
  "Aliases": "ALIASES",
  "bool": "BOOL",
  "int": "INT",
  "true": "TRUE",
  "false": "FALSE",
  "NW": "NW",
  "N": "N",
  "NE": "NE",
  "W": "W",
  "E": "E",
  "SW": "SW",
  "S": "S",
  "SE": "SE",
  "neighbours": "NEIGHBOURS",
  "row": "ROW",
  "col": "COL",
}

tokens = (
  "META",
  "CELL",
  "SELECTORS",
  "MATCHCOUNT",
  "RULES",
  "ALIASES",
  "BOOL",
  "INT",
  "TRUE",
  "FALSE",
  "NW", "N", "NE",
  "W", "E",
  "SW", "S", "SE",
  "NEIGHBOURS",
  "ROW",
  "COL",
  "LPAREN",
  "RPAREN",
  "LBRACE",
  "RBRACE",
  "LSQBRACE",
  "RSQBRACE",
  "EQUALS",
  "ASSIGN",
  "NOT",
  "PLUS",
  "MINUS",
  "MULTIPLY",
  "DIVIDE",
  "MODULO",
  "AND",
  "OR",
  "GREATER",
  "LESS",
  "GREATEREQ",
  "LESSEQ",
  "NUMBER",
  "COMMA",
  "NAME",
)

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

def t_MODULO(t):
  r"%"
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
  t.type = reserved.get(t.value, "NAME")
  return t

def t_newline(t):
  r"\n+"
  t.lexer.lineno += t.value.count("\n")

t_ignore = " \t"

def t_error(t):
  print(f"Illegal character {t.value[0]!r}")
  t.lexer.skip(1)


import ply.lex as lex
from nodes import *

lex.lex()

# Same as Java precedence: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html
# PLY wants these defined in reverse order for some reason: http://www.dabeaz.com/ply/ply.html#ply_nn27
precedence = (
  ("left", "OR"),
  ("left", "AND"),
  ("left", "EQUALS"),
  ("left", "GREATER", "LESS", "GREATEREQ", "LESSEQ"),
  ("left", "PLUS", "MINUS"),
  ("left", "MULTIPLY", "DIVIDE", "MODULO"),
  ("left", "UMINUS", "NOT"),
)

def _epsilon_or_list(p):
  if len(p) <= 1:
    p[0] = []
  else:
    hd = p[1]
    tl = p[2]
    tl.insert(0, hd)
    p[0] = tl

def p_lang(p):
  """
  lang : meta cell aliases selectors rules
  """
  p[0] = ProgramNode(p[1], p[2], p[3], p[4], p[5])

def p_meta(p):
  """
  meta : META LBRACE statements RBRACE
  """
  p[0] = MetaNode(p[3])

def p_cell(p):
  """
  cell : CELL LBRACE declarations RBRACE
  """
  p[0] = CellNode(p[3])

def p_aliases(p):
  """
  aliases :
          | ALIASES LBRACE statement_groups RBRACE
  """
  stmt_grps = p[3] if len(p) >= 4 else []
  p[0] = AliasesNode(stmt_grps)

def p_selectors(p):
  """
  selectors :
            | SELECTORS LBRACE statements RBRACE
  """
  stmts = p[3] if len(p) >= 4 else []
  p[0] = SelectorsNode(stmts)

def p_rules(p):
  """
  rules :
        | RULES LBRACE rule_statements RBRACE
  """
  rules = p[3] if len(p) >= 4 else []
  p[0] = RulesNode(rules)

def p_rule_statements(p):
  """
  rule_statements :
                  | rule_statement rule_statements
  """
  _epsilon_or_list(p)

def p_rule_statement(p):
  """
  rule_statement : NAME LPAREN NAME RPAREN ASSIGN LBRACE statements RBRACE
  """
  p[0] = RuleStatementNode(ruleName = p[1], selectorName = p[3], stmts = p[7])

def p_statement_groups(p):
  """
  statement_groups :
                   | statement_group statement_groups
  """
  _epsilon_or_list(p)

def p_statement_group(p):
  """
  statement_group : NAME ASSIGN LBRACE statements RBRACE
  """
  p[0] = StatementGroupNode(name = p[1], stmts = p[4])

def p_statements(p):
  """
  statements :
             | statement statements
  """
  _epsilon_or_list(p)

def p_statement(p):
  """
  statement : NAME ASSIGN expression
  """
  p[0] = StatementNode(name = p[1], exp = p[3])

def p_declarations(p):
  """
  declarations : 
               | declaration declarations
  """
  _epsilon_or_list(p)

def p_declaration(p):
  """
  declaration : INT NAME ASSIGN expression
              | BOOL NAME ASSIGN expression
  """
  p[0] = DeclarationNode(typ = p[1], name = p[2], exp = p[4])

def p_expression(p):
  """
  expression : binop
             | bool_literal
             | int_literal
             | unaryop
             | reference
             | subexp
             | matchcount
  """
  p[0] = p[1]

def p_binop(p):
  """
  binop : expression PLUS expression
        | expression MINUS expression
        | expression MULTIPLY expression
        | expression DIVIDE expression
        | expression MODULO expression
        | expression GREATER expression
        | expression LESS expression
        | expression GREATEREQ expression
        | expression LESSEQ expression
        | expression EQUALS expression
        | expression OR expression
        | expression AND expression
  """
  p[0] = BinOpNode(left = p[1], op = p[2], right = p[3])

def p_bool_literal(p):
  """
  bool_literal : TRUE
               | FALSE
  """
  p[0] = BoolLiteralNode(value = p[1])

def p_int_literal(p):
  """
  int_literal : NUMBER
  """
  p[0] = IntLiteralNode(value = int(p[1]))

def p_unaryop(p):
  """
  unaryop : NOT expression
          | MINUS expression %prec UMINUS
  """
  p[0] = UnaryOpNode(op = p[1], exp = [2])

def p_reference(p):
  """
  reference : NAME
            | ROW
            | COL
  """
  p[0] = ReferenceNode(name = p[1])

def p_subexp(p):
  """
  subexp : LPAREN expression RPAREN
  """
  p[0] = p[2]

def p_matchcount(p):
  """
  matchcount : MATCHCOUNT LPAREN dir_list COMMA NAME RPAREN
             | MATCHCOUNT LPAREN NEIGHBOURS COMMA NAME RPAREN
  """
  dirs = p[3]
  # TODO dirs can be represented in an 8 bit mask, then neighbours == (uint) 255
  if isinstance(dirs, str) and dirs == 'neighbours':
    dir_list = ['NW', 'N', 'NE', 'W', 'E', 'SW', 'S', 'SE']
  else:
    dir_list = dirs
  p[0] = MatchCountNode(dirs = dir_list, selector = p[5])

def p_dir_list(p):
  """
  dir_list : LSQBRACE dir_list_loop RSQBRACE
  """
  p[0] = p[1]

def p_dir_list_loop(p):
  """
  dir_list_loop : direction
                | direction COMMA dir_list_loop
  """
  hd = p[1]
  try:
    tl = p[3]
  except IndexError:
    tl = []
  tl.insert(0, hd)
  p[0] = tl

def p_direction(p):
  """
  direction : NW
            | NE
            | N
            | W
            | E
            | SW
            | SE
            | S
  """
  p[0] = p[1]

def p_error(p):
  print(f"Syntax error at {p.value!r}: {repr(p)}")

import ply.yacc as yacc
yacc.yacc()

### Entry point
### Usage: python lang.py <inputFileName>
import sys
from pprint import pprint
if __name__ == '__main__':
  fileName = sys.argv[1]
  with open(fileName, "r") as f:
    code = f.read()

  ast = yacc.parse(code)
  pprint(ast)

from irgen import IRGen
IRGen(ast)
