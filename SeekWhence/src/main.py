#!/usr/bin/python3

import os

from parsy import *
from dataclasses import dataclass
from typing import Any

# Datatypes

@dataclass
class Sequence:
  exprs: tuple
  ident: str = "<anonymous>"
  baseCase: list = None

  __cache = {}

  def generator(self, ctx={}):
    current, prev = 0, 0
    while True: 
      yield (nextval := self.value_at(current, ctx, prev))
      current, prev = current+1, nextval

  def value_at(self, n, ctx={}, prev=None):
    if n < 0: return 0
    if self.baseCase is not None and n < len(self.baseCase): return self.baseCase[n]
    if n in self.__cache: return self.__cache[n]
    
    if prev == None: prev = self.value_at(n-1, ctx)
    self.__cache[n] = exec_expr(self.exprs[n % len(self.exprs)], { **ctx, 'x': prev, 'n': n, 'S': self })
    return self.__cache[n]

  def __str__(self):
    exprs = ", ".join([display_expr(e) for e in self.exprs])
    if self.baseCase is not None:
      basec = ", ".join([str(i) for i in self.baseCase])
      return f"[sequence {self.ident}: {basec} | {exprs}]"
    return f"[sequence {self.ident}: {exprs}]"

@dataclass
class Identifier:
  name: str

  def __str__(self):
    return self.name

@dataclass
class ForLoop:
  amount: Any
  srcseq: str
  ident: str
  body: Any

@dataclass
class IfStmt:
  cond: Any
  body: Any
  elseBlock: Any = None

@dataclass
class FuncDef:
  ident: str
  argNames: list
  body: Any

@dataclass
class FuncCall:
  name: str
  args: list

  def __str__(self):
    args = ", ".join([display_expr(e) for e in self.args])
    return f"[function {self.name}: {args}]"

@dataclass
class SequenceAccess:
  ident: str
  index: Any

  def __str__(self):
    index = display_expr(self.index)
    return f"{self.ident}:{index}"

@dataclass
class ReturnStmt:
  value: Any

BreakStmt    = {'__unique':"break"}
InfiniteLoop = {'__unique':':inf'}


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
  newline    = regex(r'\s*(\n\s*)+')
  validident = regex(r'[a-zA-Z_]\w*')
  identifier = validident.map(Identifier)
  integer    = regex(r'\d+').map(int)

  # Strings
  string_lit_double = regex(r'"(?:[^"\\]|\\.)*"')
  string_lit_single = regex(r"'(?:[^'\\]|\\.)*'")
  string_lit        = (string_lit_double | string_lit_single).map(lambda s: s[1:-1])

  # Expressions
  collapse_expr  = lambda l,o=None: l if o == None else (o[0], collapse_expr(l), collapse_expr(o[1]))
  expression     = forward_declaration()
  funcCall       = forward_declaration()
  sequenceAccess = seq(validident << string(':'), integer | identifier | (string('(') >> (expression | funcCall).optional() << string(')'))).combine(SequenceAccess)
  operand        = sequenceAccess | identifier | integer | string_lit | (string('(') >> (expression | funcCall).optional() << string(')'))

  mulOp = seq(padding >> char_from('*/%') << padding, operand)
  mulTm = seq(operand, mulOp.optional()).combine(collapse_expr)

  addOp = seq(padding >> char_from('+-') << padding, mulTm)
  addTm = seq(mulTm, addOp.optional()).combine(collapse_expr)

  cmpOp = seq(padding >> string_from('<=', '<', '>=', '>', '==', '!=') << padding, addTm)
  cmpTm = seq(addTm, cmpOp.optional()).combine(collapse_expr)

  expression.become(cmpTm)

  # Code blocks and statements
  block      = forward_declaration()
  statement  = forward_declaration()
  statements = newline.optional() >> padding >> statement.sep_by(newline, min=1) << padding << newline.optional()

  # Function definitions and calls
  funcDef = seq(
    _        = string('function') << whitespace,
    ident    = validident << whitespace,
    argNames = (string('with') >> whitespace >> validident.sep_by(padding >> string(',') << padding, min=1) << whitespace).optional(),
    body     = block
  ).combine_dict(FuncDef)

  funcCall.become(seq(
    name = validident << whitespace,
    args = expression.sep_by(padding >> string(',') << padding, min=0)
  ).combine_dict(FuncCall))

  # Sequence definitions
  sequenceDef = seq(
    _1       = string('sequence') << whitespace,
    ident    = validident << whitespace,
    baseCase = (string('from') >> whitespace >> integer.sep_by(padding >> string(',') << padding, min=1) << whitespace).optional(),
    _2       = string('=') << whitespace,
    exprs    = expression.sep_by(padding >> string(',') << padding, min=1)
  ).combine_dict(Sequence)

  # For loops
  forLoop = seq(
    _1     = string('for') << whitespace,
    amount = (expression | string(':inf').result(InfiniteLoop)) << whitespace,
    _2     = string('of') << whitespace,
    srcseq = validident << whitespace,
    _3     = string('as') << whitespace,
    ident  = validident << whitespace,
    body   = block
  ).combine_dict(ForLoop)

  # Break and return
  breakStmt = string('break').result(BreakStmt)

  returnStmt = seq(
    _     = string('return'),
    value = (whitespace >> expression).optional()
  ).combine_dict(ReturnStmt)

  # If/else statements
  ifStmt = seq(
    _1        = string('if') << whitespace,
    cond      = expression << whitespace,
    _2        = string('then') << whitespace,
    body      = block,
    elseBlock = (whitespace >> string('else') >> whitespace >> block).optional()
  ).combine_dict(IfStmt)

  block.become((string('{') >> statements << string('}')) | statement.map(lambda s: [s]))
  statement.become(sequenceDef | forLoop | breakStmt | ifStmt | funcDef | funcCall)

  return statements


# Runtime

default_ctx = {
  'print': lambda _,*s: print(*s),
  'true': True,
  'false': False,
  'none': None
}

operators = {
  '+': lambda x,y: x+y,
  '-': lambda x,y: x-y,
  '*': lambda x,y: x*y,
  '/': lambda x,y: x/y if y != 0 else None,
  '%': lambda x,y: x%y if y != 0 else None,

  '<':  lambda x,y: x<y,
  '<=': lambda x,y: x<=y,
  '>':  lambda x,y: x>y,
  '>=': lambda x,y: x>=y,
  '==': lambda x,y: x==y,
  '!=': lambda x,y: x!=y,
}

def exec_expr(expr, ctx={}):
  '''Execute a parsed expression'''

  if isinstance(expr, int) or isinstance(expr, str) or isinstance(expr, bool): return expr
  if isinstance(expr, Identifier): return ctx[expr.name] if expr.name in ctx else None

  if isinstance(expr, SequenceAccess):
    if expr.ident not in ctx or not isinstance(ctx[expr.ident], Sequence):
      raise Exception(f"Can't access undefined sequence {expr.ident}")
    index = exec_expr(expr.index, ctx)
    if isinstance(index, float): index = int(index)
    if not isinstance(index, int):
      raise Exception(f"Can't access sequence {expr.ident} by non-integer value")
    return ctx[expr.ident].value_at(index, ctx)

  if isinstance(expr, FuncCall):
    if expr.name not in ctx or not callable(ctx[expr.name]):
      raise Exception(f"Can't call undefined function {expr.name}")
    return ctx[expr.name](ctx, *[exec_expr(a, ctx) for a in expr.args])

  if expr[0] in operators:
    return operators[expr[0]](exec_expr(expr[1], ctx), exec_expr(expr[2], ctx))

  raise Exception(f"Unknown operator {expr[0]}")

def display_expr(expr):
  '''Return a string representation of the given expression'''

  if isinstance(expr, str): 
    disp = expr.replace("\\", "\\\\").replace("\"", "\\\"")
    return f'"{disp}"'

  if isinstance(expr, tuple):
    if expr[0] in operators:
      return f"({display_expr(expr[1])} {expr[0]} {display_expr(expr[2])})"
    raise Exception("Unexpected tuple when displaying expression")

  return str(expr)

def execute(program, ctx=default_ctx, in_loop=False):
  '''Execute a list of parsed statements'''

  for statement in program:
    # Sequence definitions
    if isinstance(statement, Sequence):
      ctx[statement.ident] = statement
      continue

    # If/else statements
    if isinstance(statement, IfStmt):
      cond = exec_expr(statement.cond, ctx)
      if cond:
        value = execute(statement.body, dict(ctx), in_loop)
        if value is BreakStmt: return BreakStmt
      elif statement.elseBlock is not None:
        value = execute(statement.elseBlock, dict(ctx), in_loop)
        if value is BreakStmt: return BreakStmt

    # For loops
    if isinstance(statement, ForLoop):
      if statement.srcseq not in ctx or not isinstance(ctx[statement.srcseq], Sequence):
        raise Exception(f"Can't iterate over undefined sequence {statement.srcseq}")

      if statement.amount is InfiniteLoop:
        for value in ctx[statement.srcseq].generator():
          value = execute(statement.body, { **ctx, statement.ident: value }, True)
          if value is BreakStmt: break
      else:
        maxiters, numiters = exec_expr(statement.amount, ctx), 0
        if not isinstance(maxiters, int):
          raise Exception(f"Amount expression of for loop must evaluate to an integer")

        for value in ctx[statement.srcseq].generator():
          value = execute(statement.body, { **ctx, statement.ident: value }, True)
          if value is BreakStmt: break
          numiters += 1
          if numiters >= maxiters: break

      continue

    # Function definitions and calls
    if isinstance(statement, FuncDef):
      def create_user_func(fdef):
        def exec_user_func(ctx, *args):
          arity = len(fdef.argNames) if fdef.argNames is not None else 0
          if len(args) != arity: raise Exception(f"Function {fdef.ident} expects {arity} arguments, got {len(args)}")
          return execute(fdef.body, { **ctx, **{ fdef.argNames[i]: v for i,v in enumerate(args) } })
        return exec_user_func
      ctx[statement.ident] = create_user_func(statement)

    if isinstance(statement, FuncCall):
      exec_expr(statement, ctx)
      continue

    # Break statements
    if statement is BreakStmt:
      if not in_loop: raise Exception("Can't break outside of loop")
      return BreakStmt


# CLI

if __name__ == "__main__":
  # for filename in os.listdir('../examples'):
  #   with open(f'../examples/{filename}', 'r') as file:
  with open(f'../examples/fibonacci.seq', 'r') as file:  
      strin = decomment(file.read())
      parser = create_parser()
      execute(parser.parse(strin))