
from parsy import *
from collections import namedtuple
from codecs import escape_decode

from .sequence import Sequence, SequenceSlice
from .utils import *

Identifier   = namedtuple('ident',   ['name'])
LoopStmt     = namedtuple('loop',    ['amount', 'srcseq', 'ident', 'body'])
CondStmt     = namedtuple('cond',    ['cond', 'body', 'elseBlock'])
FuncDefn     = namedtuple('func',    ['ident', 'argNames', 'body'])
FuncCall     = namedtuple('call',    ['name', 'args'])
VarDef       = namedtuple('setv',    ['ident', 'expr'])
ReturnStmt   = namedtuple('_return', ['value'])
NamespaceDef = namedtuple('namespc', ['ident', 'body'])
ImportStmt   = namedtuple('_import', ['ident', 'path'])
BoolOp       = namedtuple('boolop',  ['op', 'lho', 'rho'])
BreakStmt    = namedtuple('_break',  [])()
InfiniteLoop = namedtuple('_inflp',  [])()


def create_parser():
  '''Create a parser instance'''

  # Basic elements
  padding     = whitespace.optional()
  newline     = regex(r'\s*(\n\s*)+').desc('newline')
  wsnonewline = regex(r'[\r\t\f\v  ]+')
  validident  = regex(r'[a-zA-Z_]\w*').desc('valid identifier')
  identifier  = seq(string('$').optional(), validident.sep_by(string('~'),min=1).map('~'.join)).map(lambda x: x[0]+x[1] if x[0] else x[1]).map(Identifier).desc('valid identifier')
  integer_lit = regex(r'-?\d+').map(int).desc('integer literal')
  float_lit   = regex(r'-?\d*\.\d+').map(float).desc('float literal')

  # Strings
  string_lit_double = regex(r'"(?:[^"\\]|\\.)*"').desc('string literal')
  string_lit_single = regex(r"'(?:[^'\\]|\\.)*'").desc('string literal')
  string_lit        = (string_lit_double | string_lit_single).map(lambda s: escape_decode(bytes(s[1:-1], 'utf-8'))[0].decode('utf-8'))

  # Expressions
  expression       = forward_declaration()
  expressionInline = string('(') >> padding >> expression.optional() << padding << string(')')
  funcCall         = forward_declaration()
  funcCallInline   = string('[') >> padding >> funcCall << padding << string(']')
  operand          = identifier | float_lit | integer_lit | string_lit | funcCallInline | expressionInline

  def collapse_preop(op, rho):
    if len(op) < 1: return rho
    if len(op) < 2: return (op[0], 0, rho)
    return (op[0], 0, collapse_preop(op[1:], rho))

  def collapse_binop(lho, opr=None, constr=mktuple):
    if opr is None: return lho
    op, rho = opr[0]
    expr = constr(op, collapse_binop(lho, None, constr), collapse_binop(rho, None, constr))
    return collapse_binop(expr, opr[1:], constr) if len(opr) > 1 else expr

  opr_terms = [operand]
  def define_binop(parse_op, suffixable=False):
    nonlocal opr_terms
    if suffixable: parse_op = seq(parse_op, char_from(':~').optional()).map(lambda x: x[0]+x[1] if x[1] else x[0])
    op_right = seq(padding >> parse_op << padding, opr_terms[-1]).at_least(1)
    opr_terms.append(seq(opr_terms[-1], op_right.optional()).combine(collapse_binop))

  define_binop(string_from('::',':'))

  opr_terms.append(seq((padding >> char_from('-') << padding).many(), opr_terms[-1]).combine(collapse_preop))

  define_binop(char_from('^'), True)
  define_binop(char_from('*/%'), True)
  define_binop(char_from('+-'), True)
  define_binop(string_from('<=', '<', '>=', '>', '==', '!='))

  opr_terms.append(alt(
    seq(opr_terms[-1], seq(padding >> string_from('and','or') << padding, opr_terms[-1]).at_least(1).optional()).combine(lambda l,o: collapse_binop(l,o,BoolOp)),
    opr_terms[-1]
  ))

  expression.become(opr_terms[-1])

  # Code blocks and statements
  block      = forward_declaration()
  statement  = forward_declaration()
  statements = newline.optional() >> padding >> statement.sep_by(newline, min=1) << padding << newline.optional()

  # Sequence definitions
  sequenceDef = seq(
    _1        = string('sequence') << whitespace,
    ident     = validident,
    sliceIndx = (string('::') >> integer_lit).optional(),
    baseCases = (whitespace >> string('from') >> whitespace >> operand.sep_by(padding >> string(',') << padding, min=1)).optional(),
    _2        = whitespace >> string('=') << whitespace,
    exprs     = expression.sep_by(padding >> string(',') << padding, min=1)
  ).combine_dict(Sequence.build)

  # Function definitions and calls
  funcDef = seq(
    _        = string('function') << whitespace,
    ident    = validident << whitespace,
    argNames = (string('with') >> whitespace >> validident.sep_by(padding >> string(',') << padding, min=1) << whitespace).optional(),
    body     = block
  ).combine_dict(FuncDefn)

  funcCall.become(seq(
    name = identifier.map(lambda i: i.name),
    args = (wsnonewline >> expression.sep_by(padding >> string(',') << padding, min=1)).optional()
  ).combine_dict(FuncCall))

  # Variable definitions
  varDef = seq(
    _1    = string('set') << whitespace,
    ident = validident << whitespace,
    _2    = string('=') << whitespace,
    expr  = expression
  ).combine_dict(VarDef)

  # For loops
  loopStmt = seq(
    _1     = string('for') << whitespace,
    amount = (expression | string(':inf').result(InfiniteLoop)) << whitespace,
    _2     = string('of') << whitespace,
    srcseq = expression << whitespace,
    _3     = string('as') << whitespace,
    ident  = validident << whitespace,
    _4     = string('do') << whitespace,
    body   = block
  ).combine_dict(LoopStmt)

  # Break and return
  breakStmt = string('break').result(BreakStmt)

  returnStmt = seq(
    _     = string('return'),
    value = (whitespace >> expression).optional()
  ).combine_dict(ReturnStmt)

  # If/else statements
  condStmt = seq(
    _1        = string('if') << whitespace,
    cond      = expression << whitespace,
    _2        = string('then') << whitespace,
    body      = block,
    elseBlock = (whitespace >> string('else') >> whitespace >> block).optional()
  ).combine_dict(CondStmt)

  # Namespace definitions and imports
  namespaceDef = seq(
    _     = string('namespace') << whitespace,
    ident = validident << whitespace,
    body  = block
  ).combine_dict(NamespaceDef)

  importStmt = seq(
    _1    = string('import') << whitespace,
    ident = validident << whitespace,
    _2    = string('from') << whitespace,
    path  = string_lit
  ).combine_dict(ImportStmt)

  block.become((string('{') >> statements << string('}')) | statement.map(lambda s: [s]))
  statement.become(sequenceDef | loopStmt | breakStmt | returnStmt | condStmt | namespaceDef | importStmt | varDef | funcDef | funcCall)

  return statements