#!/usr/bin/python3

import os
import sys
import math

from parsy import *
from dataclasses import dataclass, replace as copy_dc
from collections import namedtuple
from codecs import escape_decode
from time import time as systime
from abc import ABC, abstractmethod

# Datatypes

class SequenceLike(ABC):
  @abstractmethod
  def generator(self, ctx={}):
    '''Generator function which yields each step of the sequence from 0.'''

  @abstractmethod
  def value_at(self, n, ctx={}, prev=None):
    '''Returns the value of the step at index n, calculating the previous step if needed and prev is not passed.'''

@dataclass
class Sequence(SequenceLike):
  ident: str = "<anonymous>"
  exprs: list = None
  baseCases: list = None

  __cache = {}

  def __post_init__(self):
    self.baseCases = [simplify_expr(e) for e in self.baseCases or []]
    self.exprs = [simplify_expr(e) for e in self.exprs or []]

  def generator(self, ctx={}):
    '''Generator function which yields each step of the sequence from 0.'''
    current, prev = 0, None
    while True: 
      yield (nextval := self.value_at(current, ctx, prev))
      current, prev = current+1, nextval

  def value_at(self, n, ctx={}, prev=None):
    '''Returns the value of the step at index n, calculating the previous step if needed and prev is not passed.'''
    if n < 0: return 0
    if n in self.__cache: return self.__cache[n]

    if self.baseCases is not None and n < len(self.baseCases):
      self.__cache[n] = exec_expr(self.baseCases[n], ctx)
    else:
      if prev is None and find_in_expr(self.exprs[n % len(self.exprs)], 'x', 'S'): prev = self.value_at(n-1, ctx)
      self.__cache[n] = exec_expr(self.exprs[n % len(self.exprs)], { **ctx, 'x': prev, 'n': n, 'S': self })

    return self.__cache[n]

  def __str__(self):
    exprs = ", ".join([display_expr(e) for e in self.exprs])
    if len(self.baseCases) > 0:
      basec = ", ".join([display_expr(i) for i in self.baseCases])
      return f"[sequence {self.ident}: {basec} | {exprs}]"
    return f"[sequence {self.ident}: {exprs}]"

  @staticmethod
  def build(ident, exprs, baseCases=None, sliceIndx=None):
    '''Factory function; if passed a slice index, returns a slice of the created sequence instead.'''
    sequence = Sequence(ident, exprs, baseCases)
    return sequence if sliceIndx is None else SequenceSlice(sequence, sliceIndx)

  # Operator overloading for arithmetic operators to wrap base cases / expression
  @staticmethod
  def build_op(op, mode):
    def _sequence_op(ctx, x, y):
      if isinstance(x, Sequence): return x.__wrap_op(op, y, flip=False, mode=mode)
      if isinstance(y, Sequence): return y.__wrap_op(op, x, flip=True,  mode=mode)
      if isinstance(x, SequenceSlice): return SequenceSlice(x.__wrap_op(op, y, flip=False, mode=mode), x.sliceIndx)
      if isinstance(y, SequenceSlice): return SequenceSlice(y.__wrap_op(op, x, flip=True,  mode=mode), y.sliceIndx)
      return operators[op](x,y)
    return _sequence_op 

  def __wrap_op(self, op, opr, flip=False, mode='both'):
    if isinstance(opr, SequenceLike):
      raise Exception(f'Arithmetic operations on multiple sequences are not currently supported')
    else:
      new_exprs = [(op, opr if flip else e, e if flip else opr) for e in self.exprs]
      new_basec = [(op, opr if flip else e, e if flip else opr) for e in self.baseCases] if self.baseCases is not None else None
      op        = op + ('~' if mode == 'base' else ':' if mode == 'expr' else '')

      return copy_dc(self,
        ident = f'<{display_expr(opr)}{op}{self.ident}>' if flip else f'<{self.ident}{op}{display_expr(opr)}>',
        exprs = new_exprs if mode != 'base' else self.exprs,
        baseCases = new_basec if mode != 'expr' else self.baseCases)

  __add__     = lambda s,o: s.__wrap_op('+', o); __radd__     = lambda s,o: s.__wrap_op('+', o, True)
  __sub__     = lambda s,o: s.__wrap_op('-', o); __rsub__     = lambda s,o: s.__wrap_op('-', o, True)
  __mul__     = lambda s,o: s.__wrap_op('*', o); __rmul__     = lambda s,o: s.__wrap_op('*', o, True)
  __truediv__ = lambda s,o: s.__wrap_op('/', o); __rtruediv__ = lambda s,o: s.__wrap_op('/', o, True)
  __mod__     = lambda s,o: s.__wrap_op('%', o); __rmod__     = lambda s,o: s.__wrap_op('%', o, True)
  __pow__     = lambda s,o: s.__wrap_op('^', o); __rpow__     = lambda s,o: s.__wrap_op('^', o, True)

  def __eq__(self, other):
    if not isinstance(other, SequenceLike): return False
    basec = all(expr_equal(a,b) for a,b in zip(self.baseCases or [], other.baseCases or []))
    exprs = all(expr_equal(a,b) for a,b in zip(self.exprs or [], other.exprs or []))
    slidx = not isinstance(other, SequenceSlice) or other.sliceIndx == 0
    return basec and exprs and slidx

@dataclass
class SequenceSlice(SequenceLike):
  sequence: SequenceLike
  sliceIndx: int

  @property 
  def ident(self): return self.sequence.ident
  @property
  def exprs(self): return self.sequence.exprs
  @property 
  def baseCases(self): return self.sequence.baseCases

  def __post_init__(self):
    if isinstance(self.sequence, SequenceSlice):
      self.sliceIndx = self.sliceIndx + self.sequence.sliceIndx
      self.sequence  = self.sequence.sequence
    if self.sliceIndx < 0:
      raise Exception(f"Can't create a slice which starts at negative indices of its base sequence")

  def generator(self, ctx={}):
    '''Generator function which yields each step of the sequence from 0.'''
    current, prev = max(self.sliceIndx, 0), None
    while True: 
      yield (nextval := self.sequence.value_at(current, ctx, prev))
      current, prev = current+1, nextval

  def value_at(self, n, ctx={}, prev=None):
    '''Returns the value of the step at index n, calculating the previous step if needed and prev is not passed.'''
    return self.sequence.value_at(n+self.sliceIndx, ctx)

  def __str__(self):
    return f"[slice {self.ident}::{self.sliceIndx}]"

  # Pass arithmetic operators through to the underlying sequence, wrapping the result in a new slice
  def __pass_op(get_op): 
    return lambda self,other: SequenceSlice(get_op(self.sequence)(other), self.sliceIndx)

  __add__     = __pass_op(lambda s: s.__add__);     __radd__     = __pass_op(lambda s: s.__radd__)
  __sub__     = __pass_op(lambda s: s.__sub__);     __rsub__     = __pass_op(lambda s: s.__rsub__)
  __mul__     = __pass_op(lambda s: s.__mul__);     __rmul__     = __pass_op(lambda s: s.__rmul__)
  __truediv__ = __pass_op(lambda s: s.__truediv__); __rtruediv__ = __pass_op(lambda s: s.__rtruediv__)
  __mod__     = __pass_op(lambda s: s.__mod__);     __rmod__     = __pass_op(lambda s: s.__rmod__)
  __pow__     = __pass_op(lambda s: s.__pow__);     __rpow__     = __pass_op(lambda s: s.__rpow__)

  def __eq__(self, other):
    if not isinstance(other, SequenceLike): return False
    if isinstance(other, Sequence): return self.sliceIndx == 0 and self.sequence == other
    return self.sliceIndx == other.sliceIndx and self.sequence == other.sequence

Identifier   = namedtuple('ident',   ['name'])
LoopStmt     = namedtuple('loop',    ['amount', 'srcseq', 'ident', 'body'])
CondStmt     = namedtuple('cond',    ['cond', 'body', 'elseBlock'])
FuncDefn     = namedtuple('func',    ['ident', 'argNames', 'body'])
FuncCall     = namedtuple('call',    ['name', 'args'])
VarDef       = namedtuple('setv',    ['ident', 'expr'])
ReturnStmt   = namedtuple('_return', ['value'])
BoolOp       = namedtuple('boolop',  ['op', 'lho', 'rho'])
BreakStmt    = namedtuple('_break',  [])()
InfiniteLoop = namedtuple('_inflp',  [])()


# Parser

def decomment(file):
  lines, out, lastc, instr = file.splitlines(), [], '', False
  for line in lines: 
    outline = ''
    for c in line:
      if c in '"\'' and lastc != '\\': instr = not instr
      if c in '\\' and lastc == '\\': lastc = ''; continue
      if c in ';' and not instr: break
      outline += c
      lastc = c
    out.append(outline)
  return '\n'.join(out)

def create_parser():
  # Basic elements
  padding    = whitespace.optional()
  newline    = regex(r'\s*(\n\s*)+').desc('newline')
  validident = regex(r'[a-zA-Z_]\w*').desc('valid identifier')
  identifier = validident.map(Identifier)
  integer    = regex(r'-?\d+').map(int).desc('integer')

  # Strings
  string_lit_double = regex(r'"(?:[^"\\]|\\.)*"').desc('string literal')
  string_lit_single = regex(r"'(?:[^'\\]|\\.)*'").desc('string literal')
  string_lit        = (string_lit_double | string_lit_single).map(lambda s: escape_decode(bytes(s[1:-1], 'utf-8'))[0].decode('utf-8'))

  # Expressions
  expression       = forward_declaration()
  expressionInline = string('(') >> padding >> expression.optional() << padding << string(')')
  funcCall         = forward_declaration()
  funcCallInline   = string('[') >> padding >> funcCall << padding << string(']')
  operand          = identifier | integer | string_lit | funcCallInline | expressionInline

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
    sliceIndx = (string('::') >> integer).optional(),
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
    name = validident,
    args = (whitespace >> expression.sep_by(padding >> string(',') << padding, min=1)).optional()
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

  block.become((string('{') >> statements << string('}')) | statement.map(lambda s: [s]))
  statement.become(sequenceDef | loopStmt | breakStmt | returnStmt | condStmt | varDef | funcDef | funcCall)

  return statements


# Runtime

default_ctx = {
  'print': lambda _,*s: print(*[(str(x).lower() if x in [True,False,None] else x) for x in s]),
  'input': lambda _,p=None: (input() if p is None else input(p)) if sys.stdin.isatty() else sys.stdin.readline().rstrip('\n'),
  'systime': lambda _: math.floor(systime()*1000),

  'floor': lambda _,x: math.floor(x),
  'ceil': lambda _,x: math.ceil(x),
  'round': lambda _,x: math.round(x),

  'unslice': lambda _,x: x.sequence if isinstance(x,SequenceSlice) else x if isinstance(x,Sequence) else None,

  'true': True,
  'false': False,
  'none': None,
  'not': lambda _,x: not x
}

operators = {
  # Arithmetic
  '+': lambda _,x,y: x+y,
  '-': lambda _,x,y: x-y,
  '*': lambda _,x,y: x*y,
  '/': lambda _,x,y: x/y if y != 0 else None,
  '%': lambda _,x,y: x%y if y != 0 else None,
  '^': lambda _,x,y: x**y,

  # Sequence-specialized arithmetic
  '+~': Sequence.build_op('+', "base"), '+:': Sequence.build_op('+', "expr"),
  '-~': Sequence.build_op('-', "base"), '-:': Sequence.build_op('-', "expr"),
  '*~': Sequence.build_op('*', "base"), '*:': Sequence.build_op('*', "expr"),
  '/~': Sequence.build_op('/', "base"), '/:': Sequence.build_op('/', "expr"),
  '%~': Sequence.build_op('%', "base"), '%:': Sequence.build_op('%', "expr"),
  '^~': Sequence.build_op('^', "base"), '^:': Sequence.build_op('^', "expr"),

  # Comparison
  '<':  lambda _,x,y: x<y,
  '<=': lambda _,x,y: x<=y,
  '>':  lambda _,x,y: x>y,
  '>=': lambda _,x,y: x>=y,
  '==': lambda _,x,y: x==y,
  '!=': lambda _,x,y: x!=y,

  # Sequence access / slicing
  ':': lambda c,x,y: x.value_at(math.floor(y),c) if isinstance(x,SequenceLike) and isnumber(y) else None,
  '::': lambda _,x,y: SequenceSlice(x,math.floor(y)) if isinstance(x,SequenceLike) and isnumber(y) else None,
}

def exec_expr(expr, ctx={}):
  '''Execute a parsed expression.'''

  if isinstance(expr, Identifier): 
    return ctx[expr.name] if expr.name in ctx else None

  if isinstance(expr, FuncCall):
    if expr.name not in ctx or not callable(ctx[expr.name]):
      raise Exception(f"Can't call undefined function {expr.name}")
    return ctx[expr.name](ctx, *[exec_expr(a, ctx) for a in expr.args or []])

  if isinstance(expr, BoolOp):
    lhr = exec_expr(expr.lho, ctx)
    if expr.op == 'and' and not bool(lhr): return False
    if expr.op == 'or' and bool(lhr): return True
    rhr = exec_expr(expr.rho, ctx)
    return bool(rhr)

  if istuple(expr):
    if expr[0] in operators:
      return operators[expr[0]](ctx, exec_expr(expr[1], ctx), exec_expr(expr[2], ctx))
    raise Exception(f"Unknown operator {expr[0]}")

  return expr

def display_expr(expr):
  '''Return a string representation of the given expression.'''

  if isinstance(expr, str): 
    disp = expr.replace("\\", "\\\\")
    disp = expr.replace("\"", "\\\"")
    disp = expr.replace("\n", "\\\n")
    disp = expr.replace("\r", "\\\r")
    disp = expr.replace("\t", "\\\t")
    disp = expr.replace("\b", "\\\b")
    disp = expr.replace("\f", "\\\f")
    return f'"{disp}"'

  if expr is None or isinstance(expr, bool):
    return str(expr).lower()

  if isinstance(expr, Identifier):
    return expr.name

  if isinstance(expr, FuncCall):
    if expr.args is not None:
      args = ", ".join([display_expr(e) for e in expr.args])
      return f"[call_function {expr.name}: {args}]"
    return f"[call_function {expr.name}]"

  if isinstance(expr, BoolOp):
    return f'({display_expr(expr.lho)} {expr.op} {display_expr(expr.rho)})'

  if istuple(expr):
    if expr[0] == ':':
      return f"{display_expr(expr[1])}:{display_expr(expr[2])}"
    if expr[0] == '::':
      return f"{display_expr(expr[1])}::{display_expr(expr[2])}"
    if expr[0] in operators:
      if expr[0] == '-' and expr[1] == 0: return f"-{display_expr(expr[1])}"
      return f"({display_expr(expr[1])} {expr[0]} {display_expr(expr[2])})"
    raise Exception("Unexpected tuple when displaying expression")

  return str(expr)

def find_in_expr(expr, *idns):
  '''Return true if any of the given identifiers or constant numbers are found in the given expression.'''

  if isinstance(expr, int): return expr in idns
  if isinstance(expr, Identifier): return expr.name in idns
  if isinstance(expr, FuncCall): 
    return expr.name in idns or any(find_in_expr(e, *idns) for e in expr.args)
  if isinstance(expr, tuple):
    return find_in_expr(expr[1], *idns) or find_in_expr(expr[2], *idns)

  return False

def expr_equal(a, b):
  '''Check if two expressions are semantically equivalent (!= equal after evaluation).'''

  if a == b: return True
  if type(a) != type(b): return False

  if isinstance(a, Identifier):
    return a.name == b.name
  if isinstance(a, FuncCall):
    return a.name == b.name and all(expr_equal(aa,ba) for aa,ba in zip(a.args or [], b.args or []))

  if isinstance(a, tuple):
    if a[0] != b[0]: return False
    if expr_equal(a[1], b[1]) and expr_equal(a[2], b[2]): return True
    if expr_equal(a[1], b[2]) and expr_equal(a[2], b[1]): return a[0][0] in '+*=!'
    return False

  return False

def simplify_expr(expr):
  '''Simplify an expression if possible via constant and operation folding.'''

  if not istuple(expr) or expr[0] not in operators: return expr

  lho  = simplify_expr(expr[1])
  rho  = simplify_expr(expr[2])
  expr = (expr[0], lho, rho)

  if isnumber(lho) and isnumber(rho):
    return operators[expr[0]]({}, lho, rho)

  collapse_bl = lambda l,r,o: istuple(l) and istuple(r) and l[0] == r[0] == o and isnumber(l[2]) and isnumber(r[2]) and expr_equal(l[1], r[1])
  collapse_br = lambda l,r,o: istuple(l) and istuple(r) and l[0] == r[0] == o and isnumber(l[1]) and isnumber(r[1]) and expr_equal(l[2], r[2])
  collapse_il = lambda l,r,o: istuple(r) and r[0] == o and isnumber(r[2]) and expr_equal(l, r[1])
  collapse_ir = lambda l,r,o: istuple(r) and r[0] == o and isnumber(r[1]) and expr_equal(l, r[2])
  collapse_cl = lambda l,r,o: istuple(r) and r[0] == o and isnumber(r[1]) and isnumber(l)
  collapse_cr = lambda l,r,o: istuple(r) and r[0] == o and isnumber(r[2]) and isnumber(l)
  collapse_ng = lambda l,r: isnumber(r) and r < 0

  # Simplify multiplication by merging constants and increasing exponents
  if expr[0] == '*':
    if collapse_bl(lho, rho, '^'): return simplify_expr(('^', lho[1], lho[2]+rho[2]))
    if collapse_il(lho, rho, '^'): return simplify_expr(('^', rho[1], rho[2]+1))
    if collapse_il(rho, lho, '^'): return simplify_expr(('^', lho[1], lho[2]+1))
    if expr_equal(lho, rho):       return simplify_expr(('^', lho, 2))

    if collapse_cl(lho, rho, '*'): return simplify_expr(('*', rho[2], lho+rho[1]))
    if collapse_cl(rho, lho, '*'): return simplify_expr(('*', lho[2], lho[1]+rho))
    if collapse_cr(lho, rho, '*'): return simplify_expr(('*', rho[1], lho+rho[2]))
    if collapse_cr(rho, lho, '*'): return simplify_expr(('*', lho[1], lho[2]+rho))

  # Simplify addition by merging constants, increasing coefficients, and flattening with subtraction/negation
  if expr[0] == '+':
    if collapse_bl(lho, rho, '*'): return simplify_expr(('*', lho[1], lho[2]+rho[2]))
    if collapse_br(lho, rho, '*'): return simplify_expr(('*', lho[2], lho[1]+rho[1]))
    if collapse_il(lho, rho, '*'): return simplify_expr(('*', rho[1], rho[2]+1))
    if collapse_il(rho, lho, '*'): return simplify_expr(('*', lho[1], lho[2]+1))
    if collapse_ir(lho, rho, '*'): return simplify_expr(('*', rho[2], rho[1]+1))
    if collapse_ir(rho, lho, '*'): return simplify_expr(('*', lho[2], lho[1]+1))
    if expr_equal(lho, rho):       return simplify_expr(('*', lho, 2))

    if collapse_cl(lho, rho, '+'): return simplify_expr(('+', rho[2], lho+rho[1]))
    if collapse_cl(rho, lho, '+'): return simplify_expr(('+', lho[2], lho[1]+rho))
    if collapse_cr(lho, rho, '+'): return simplify_expr(('+', rho[1], lho+rho[2]))
    if collapse_cr(rho, lho, '+'): return simplify_expr(('+', lho[1], lho[2]+rho))

    if collapse_cl(lho, rho, '-'): return simplify_expr(('-', lho+rho[1], rho[2]))
    if collapse_cl(rho, lho, '-'): return simplify_expr(('-', rho+lho[1], lho[2]))
    if collapse_cr(lho, rho, '-'): return simplify_expr(('+', rho[1], lho-rho[2]))
    if collapse_cr(rho, lho, '-'): return simplify_expr(('+', lho[1], rho-lho[2]))

    if collapse_ng(lho, rho): return simplify_expr(('-', lho, -rho))
    if collapse_ng(rho, lho): return simplify_expr(('-', rho, -lho))

  # Simplify subtraction by merging constants and flattening with addition/negation
  if expr[0] == '-':
    if collapse_cl(lho, rho, '+'): return simplify_expr(('-', lho-rho[1], rho[2]))
    if collapse_cl(rho, lho, '+'): return simplify_expr(('+', lho[1]-rho, lho[2]))
    if collapse_cr(lho, rho, '+'): return simplify_expr(('-', lho-rho[2], rho[1]))
    if collapse_cr(rho, lho, '+'): return simplify_expr(('+', lho[2]-rho, lho[1]))

    if collapse_cl(lho, rho, '-'): return simplify_expr(('+', lho-rho[1], rho[2]))
    if collapse_cl(rho, lho, '-'): return simplify_expr(('-', lho[1]-rho, lho[2]))
    if collapse_cr(lho, rho, '-'): return simplify_expr(('-', lho+rho[2], rho[1]))
    if collapse_cr(rho, lho, '-'): return simplify_expr(('-', lho[1], lho[2]+rho))

    if collapse_ng(lho, rho): return simplify_expr(('+', lho, -rho))
    if collapse_ng(rho, lho): return simplify_expr(('+', rho, -lho))

  return expr

def execute(program, ctx=dict(default_ctx), in_loop=False, in_func=False):
  '''Execute a list of parsed statements'''

  for statement in program:
    # Sequence definitions
    if isinstance(statement, SequenceLike):
      ctx[statement.ident] = statement
      continue

    # If/else statements
    if isinstance(statement, CondStmt):
      cond = exec_expr(statement.cond, ctx)
      if cond:
        value = execute(statement.body, ctx, in_loop, in_func)
        if value is BreakStmt: return BreakStmt
      elif statement.elseBlock is not None:
        value = execute(statement.elseBlock, ctx, in_loop, in_func)
        if value is BreakStmt: return BreakStmt

    # For loops
    if isinstance(statement, LoopStmt):
      srcseq = exec_expr(statement.srcseq, ctx)

      if not srcseq or not isinstance(srcseq, SequenceLike):
        raise Exception(f"Can't iterate over undefined sequence {statement.srcseq}")

      if statement.amount is InfiniteLoop:
        for value in srcseq.generator(ctx):
          value = execute(statement.body, { **ctx, statement.ident: value }, True, in_func)
          if value is BreakStmt: break
      else:
        maxiters, numiters = exec_expr(statement.amount, ctx), 0
        if not isinstance(maxiters, int):
          raise Exception(f"Amount expression of for loop must evaluate to an integer")

        for value in srcseq.generator(ctx):
          value = execute(statement.body, { **ctx, statement.ident: value }, True, in_func)
          if value is BreakStmt: break
          numiters += 1
          if numiters >= maxiters: break

      continue

    # Function definitions and calls
    if isinstance(statement, FuncDefn):
      def create_user_func(fdef):
        def exec_user_func(ctx, *args):
          arity = len(fdef.argNames) if fdef.argNames is not None else 0
          if len(args) != arity: raise Exception(f"Function {fdef.ident} expects {arity} arguments, got {len(args)}")
          return execute(fdef.body, { **ctx, **{ fdef.argNames[i]: v for i,v in enumerate(args) } }, False, True)
        return exec_user_func
      ctx[statement.ident] = create_user_func(statement)

    if isinstance(statement, FuncCall):
      exec_expr(statement, ctx)
      continue

    if isinstance(statement, VarDef):
      ctx[statement.ident] = exec_expr(statement.expr, ctx)
      continue

    # Break statements
    if statement is BreakStmt:
      if not in_loop: raise Exception("Can't break outside of loop")
      return BreakStmt

    # Return statements
    if isinstance(statement, ReturnStmt):
      if not in_func: raise Exception("Can't return outside of function")
      return exec_expr(statement.value, ctx)


# Utils

def isnumber(x):
  return isinstance(x,int) or isinstance(x,float)

def istuple(x):
  return isinstance(x, tuple) and not isnamedtuple(x)

def isnamedtuple(x):
  return isinstance(x, tuple) and hasattr(x, '_asdict') and hasattr(x, '_fields')

def mktuple(*xs):
  return tuple(xs)


# CLI

def usage():
  print(f'Usage: {sys.argv[0]} [script_path]')
  sys.exit()

if __name__ == "__main__":
  if len(sys.argv) < 2: usage()

  with open(sys.argv[1], 'r') as file:  
    strin = decomment(file.read())
    parser = create_parser()
    execute(parser.parse(strin))