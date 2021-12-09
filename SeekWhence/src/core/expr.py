
import math

from codecs import escape_decode

from .sequence import Sequence, SequenceSlice, SequenceLike
from .parser import *
from .utils import *

operators = {
  # Arithmetic
  '+': lambda _,x,y: str(x)+str(y) if isinstance(x,str) or isinstance(y,str) else x+y,
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

def exec_expr(expr, ctx):
  '''Execute a parsed expression.'''
  from .runtime import find_ident

  if isinstance(expr, Identifier): 
    return find_ident(expr.name, ctx)

  if isinstance(expr, FuncCall):
    func = find_ident(expr.name, ctx)
    if not callable(func): raise Exception(f"Can't call undefined function {expr.name}")
    return func(ctx, *[exec_expr(a, ctx) for a in expr.args or []])

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