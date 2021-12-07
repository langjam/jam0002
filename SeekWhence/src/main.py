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
    pass

  @abstractmethod
  def value_at(self, n, ctx={}, prev=None):
    pass

@dataclass
class Sequence(SequenceLike):
  exprs: tuple
  ident: str = "<anonymous>"
  baseCase: list = None
  baseIndx: int = 0

  __cache = {}

  def __post_init__(self):
    if self.baseIndx is None: self.baseIndx = 0
    if self.baseIndx < 0 and abs(self.baseIndx) > len(self.baseCase):
      raise Exception(f'Cannot define sequence {self.ident} with negative indices not covered by its base cases')

  def generator(self, ctx={}):
    current, prev = max(self.baseIndx or 0, 0), None
    while True: 
      yield (nextval := self.value_at(current, ctx, prev))
      current, prev = current+1, nextval

  def value_at(self, n, ctx={}, prev=None):
    if n < self.baseIndx: return 0
    if n in self.__cache: return self.__cache[n]

    if self.baseCase is not None and n < len(self.baseCase)+self.baseIndx:
      if self.baseIndx < 0:
        self.__cache[n] = exec_expr(self.baseCase[n-self.baseIndx], ctx)
      else:
        self.__cache[n] = exec_expr(self.baseCase[n], ctx)
    else:
      if prev is None and find_in_expr(self.exprs[n % len(self.exprs)], 'x', 'S'): prev = self.value_at(n-1, ctx)
      self.__cache[n] = exec_expr(self.exprs[n % len(self.exprs)], { **ctx, 'x': prev, 'n': n, 'S': self })

    return self.__cache[n]

  def __str__(self):
    exprs = ", ".join([display_expr(e) for e in self.exprs])
    if self.baseCase is not None:
      basec = ", ".join([display_expr(i) for i in self.baseCase])
      return f"[sequence {self.ident}: {basec} | {exprs}]"
    return f"[sequence {self.ident}: {exprs}]"

  def wrap_op(self, op, opr, flip=False, mode='both'):
    if isinstance(opr, SequenceLike):
      raise Exception(f'Arithmetic operations on multiple sequences are not currently supported')
    else:
      new_exprs = [(op, opr if flip else e, e if flip else opr) for e in self.exprs]
      new_basec = [(op, opr if flip else e, e if flip else opr) for e in self.baseCase] if self.baseCase is not None else None

      return copy_dc(self,
        ident = f'<{display_expr(opr)}{op}{self.ident}>' if flip else f'<{self.ident}{op}{display_expr(opr)}>',
        exprs = new_exprs if mode != 'base' else self.exprs,
        baseCase = new_basec if mode != 'expr' else self.baseCase)

  def __add__(self, other): return self.wrap_op('+', other)
  def __sub__(self, other): return self.wrap_op('-', other)
  def __mul__(self, other): return self.wrap_op('*', other)
  def __truediv__(self, other): return self.wrap_op('/', other)
  def __mod__(self, other): return self.wrap_op('%', other)
  def __pow__(self, other): return self.wrap_op('^', other)

  def __radd__(self, other): return self.wrap_op('+', other, True)
  def __rsub__(self, other): return self.wrap_op('-', other, True)
  def __rmul__(self, other): return self.wrap_op('*', other, True)
  def __rtruediv__(self, other): return self.wrap_op('/', other, True)
  def __rmod__(self, other): return self.wrap_op('%', other, True)
  def __rpow__(self, other): return self.wrap_op('^', other, True)

@dataclass
class SequenceSlice(SequenceLike):
  sequence: Sequence
  sliceIndx: int

  def generator(self, ctx={}):
    current, prev = max(self.sliceIndx, 0), 0
    while True: 
      yield (nextval := self.sequence.value_at(current, ctx, prev))
      current, prev = current+1, nextval

  def value_at(self, n, ctx={}, prev=None):
    return self.sequence.value_at(n+self.sliceIndx, ctx)

  def __str__(self):
    return f"[slice {self.sequence.ident}::{self.sliceIndx}]"

Identifier   = namedtuple('ident',   ['name'])
LoopStmt     = namedtuple('loop',    ['amount', 'srcseq', 'ident', 'body'])
CondStmt     = namedtuple('cond',    ['cond', 'body', 'elseBlock'])
FuncDefn     = namedtuple('func',    ['ident', 'argNames', 'body'])
FuncCall     = namedtuple('call',    ['name', 'args'])
VarDef       = namedtuple('setv',    ['ident', 'expr'])
ReturnStmt   = namedtuple('_return', ['value'])
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

  def collapse_expr(lho, opr=None):
    if opr is None: return lho
    op, rho = opr[0]
    expr = (op, collapse_expr(lho), collapse_expr(rho))
    return collapse_expr(expr, opr[1:]) if len(opr) > 1 else expr

  opr_terms = [operand]
  def define_opr(parse_op, suffixable=False):
    nonlocal opr_terms
    if suffixable: parse_op = seq(parse_op, char_from(':~').optional()).map(lambda x: x[0]+x[1] if x[1] else x[0])
    op_right = seq(padding >> parse_op << padding, opr_terms[-1]).at_least(1)
    opr_terms.append(seq(opr_terms[-1], op_right.optional()).combine(collapse_expr))

  define_opr(string_from('::',':'))
  define_opr(char_from('^'), True)
  define_opr(char_from('*/%'), True)
  define_opr(char_from('+-'), True)
  define_opr(string_from('<=', '<', '>=', '>', '==', '!='))

  expression.become(opr_terms[-1])

  # Code blocks and statements
  block      = forward_declaration()
  statement  = forward_declaration()
  statements = newline.optional() >> padding >> statement.sep_by(newline, min=1) << padding << newline.optional()

  # Sequence definitions
  sequenceDef = alt(
    seq(
      _1    = string('sequence') << whitespace,
      ident = validident << whitespace,
      _2    = string('=') << whitespace,
      exprs = expression.sep_by(padding >> string(',') << padding, min=1)
    ).combine_dict(Sequence),
    seq(
      _1       = string('sequence') << whitespace,
      ident    = validident << whitespace,
      baseCase = string('from') >> whitespace >> operand.sep_by(padding >> string(',') << padding, min=1) << whitespace,
      baseIndx = (string('at') >> whitespace >> integer << whitespace).optional(),
      _2       = string('=') << whitespace,
      exprs    = expression.sep_by(padding >> string(',') << padding, min=1)
    ).combine_dict(Sequence)
  )

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
  'print': lambda _,*s: print(*s),
  'input': lambda _,p=None: (input() if p is None else input(p)) if sys.stdin.isatty() else sys.stdin.readline().rstrip('\n'),
  'systime': lambda _: math.floor(systime()*1000),
  'floor': lambda _,x: math.floor(x),
  'ceil': lambda _,x: math.ceil(x),
  'round': lambda _,x: math.round(x),
  'true': True,
  'false': False,
  'none': None
}

operators = {
  '+': lambda _,x,y: x+y,
  '-': lambda _,x,y: x-y,
  '*': lambda _,x,y: x*y,
  '/': lambda _,x,y: x/y if y != 0 else None,
  '%': lambda _,x,y: x%y if y != 0 else None,
  '^': lambda _,x,y: x**y,

  '+~': lambda _,x,y: x.wrap_op('+', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['+'](x,y),
  '-~': lambda _,x,y: x.wrap_op('-', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['-'](x,y),
  '*~': lambda _,x,y: x.wrap_op('*', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['*'](x,y),
  '/~': lambda _,x,y: x.wrap_op('/', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['/'](x,y),
  '%~': lambda _,x,y: x.wrap_op('%', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['%'](x,y),
  '^~': lambda _,x,y: x.wrap_op('^', y, flip=isinstance(y,Sequence), mode="base") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['^'](x,y),

  '+:': lambda _,x,y: x.wrap_op('+', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['+'](x,y),
  '-:': lambda _,x,y: x.wrap_op('-', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['-'](x,y),
  '*:': lambda _,x,y: x.wrap_op('*', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['*'](x,y),
  '/:': lambda _,x,y: x.wrap_op('/', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['/'](x,y),
  '%:': lambda _,x,y: x.wrap_op('%', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['%'](x,y),
  '^:': lambda _,x,y: x.wrap_op('^', y, flip=isinstance(y,Sequence), mode="expr") if isinstance(x,Sequence) or isinstance(y,Sequence) else operators['^'](x,y),

  '<':  lambda _,x,y: x<y,
  '<=': lambda _,x,y: x<=y,
  '>':  lambda _,x,y: x>y,
  '>=': lambda _,x,y: x>=y,
  '==': lambda _,x,y: x==y,
  '!=': lambda _,x,y: x!=y,

  ':': lambda c,x,y: x.value_at(math.floor(y),c) if isinstance(x,SequenceLike) and (isinstance(y,int) or isinstance(y,float)) else None,
  '::': lambda _,x,y: SequenceSlice(x,y) if isinstance(x,SequenceLike) and (isinstance(y,int) or isinstance(y,float)) else None
}

def exec_expr(expr, ctx={}):
  '''Execute a parsed expression'''

  if isinstance(expr, Identifier): 
    return ctx[expr.name] if expr.name in ctx else None

  if isinstance(expr, FuncCall):
    if expr.name not in ctx or not callable(ctx[expr.name]):
      raise Exception(f"Can't call undefined function {expr.name}")
    return ctx[expr.name](ctx, *[exec_expr(a, ctx) for a in expr.args or []])

  if isinstance(expr, tuple):
    if expr[0] in operators:
      return operators[expr[0]](ctx, exec_expr(expr[1], ctx), exec_expr(expr[2], ctx))
    raise Exception(f"Unknown operator {expr[0]}")

  return expr

def display_expr(expr):
  '''Return a string representation of the given expression'''

  if isinstance(expr, str): 
    disp = expr.replace("\\", "\\\\")
    disp = expr.replace("\"", "\\\"")
    disp = expr.replace("\n", "\\\n")
    disp = expr.replace("\r", "\\\r")
    disp = expr.replace("\t", "\\\t")
    disp = expr.replace("\b", "\\\b")
    disp = expr.replace("\f", "\\\f")
    return f'"{disp}"'

  if isinstance(expr, Identifier):
    return expr.name

  if isinstance(expr, FuncCall):
    if expr.args is not None:
      args = ", ".join([display_expr(e) for e in expr.args])
      return f"[call_function {expr.name}: {args}]"
    return f"[call_function {expr.name}]"

  if isinstance(expr, tuple):
    if expr[0] == ':':
      return f"{display_expr(expr[1])}:{display_expr(expr[2])}"
    if expr[0] == '::':
      return f"{display_expr(expr[1])}::{display_expr(expr[2])}"
    if expr[0] in operators:
      return f"({display_expr(expr[1])} {expr[0]} {display_expr(expr[2])})"
    raise Exception("Unexpected tuple when displaying expression")

  return str(expr)

def find_in_expr(expr, *idns):
  '''Return true if any of the given identifiers or constant numbers are found in the given expression'''

  if isinstance(expr, int): return expr in idns
  if isinstance(expr, Identifier): return expr.name in idns
  if isinstance(expr, FuncCall): 
    return expr.name in idns or any(find_in_expr(e, *idns) for e in expr.args)
  if isinstance(expr, tuple):
    return find_in_expr(expr[1], *idns) or find_in_expr(expr[2], *idns)

  return False

def execute(program, ctx=default_ctx, in_loop=False, in_func=False):
  '''Execute a list of parsed statements'''

  for statement in program:
    # Sequence definitions
    if isinstance(statement, Sequence):
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