import os
import sys
import math

from time import time as systime
from importlib import import_module

from .sequence import *
from .parser import *
from .utils import *
from .expr import *

from stdlib.__builtin__ import SEQ_EXPORTS as default_ctx

def execute(program, ctx=None, in_loop=False, in_func=False):
  '''Execute a list of parsed statements'''
  if ctx is None: ctx = mkctx(None)

  for statement in program:
    # Sequence definitions
    if isinstance(statement, SequenceLike):
      ctx[statement.ident] = statement
      continue

    # Namespace definitions
    if isinstance(statement, NamespaceDef):
      existing = find_ident(statement.ident, ctx)
      existing = existing if existing is not None else {}

      if not isinstance(existing, dict):
        raise Exception(f"Can't define namespace {statement.ident}; that name is already in use by a non-namespace")

      inner_ctx = mkctx(ctx, { **existing, '$$namespace_ident': statement.ident })
      execute(statement.body, inner_ctx)
      ctx[statement.ident] = inner_ctx
      continue

    # Imports
    if isinstance(statement, ImportStmt):
      existing = find_ident(statement.ident, ctx)
      if existing is not None:
        raise Exception(f"Can't import namespace {statement.ident}; that name is already in use")

      def _do_import(path, ctx):
        script_path = find_ident('$file', ctx)
        local_path  = os.path.realpath(find_ident('$dir', ctx)+'/'+path)
        stdlib_path = os.path.realpath(find_ident('$stdlib', ctx)+'/'+path)

        if local_path+'.seq' != script_path and os.path.exists(local_path+'.seq'):
          return exec_file(local_path+'.seq', False)

        # if os.path.exists(local_path+'.py'):   return import_module(local_path).SEQ_EXPORTS    # Local .py imports are currently unsupported
        if os.path.exists(stdlib_path+'.seq'): return exec_file(stdlib_path+'.seq', False)
        if os.path.exists(stdlib_path+'.py'):  return import_module('stdlib.'+path).SEQ_EXPORTS

        raise Exception(f"Can't import file {path}; file not found")

      ctx[statement.ident] = _do_import(statement.path, ctx)
      ctx[statement.ident]['$$namespace_ident'] = statement.ident
      continue

    # If/else statements
    if isinstance(statement, CondStmt):
      cond = exec_expr(statement.cond, ctx)
      body = statement.body if cond else statement.elseBlock
      if body is not None:
        value = execute(body, ctx, in_loop, in_func)
        if value is BreakStmt: return BreakStmt
      continue

    # For loops
    if isinstance(statement, LoopStmt):
      srcseq = exec_expr(statement.srcseq, ctx)

      if not srcseq:
        raise Exception(f"Can't iterate over undefined sequence {display_expr(srcseq)}")
      if not isinstance(srcseq, SequenceLike):
        raise Exception(f"Can't iterate over non-sequence {display_expr(srcseq)}")

      maxiters, numiters = None if statement.amount is InfiniteLoop else exec_expr(statement.amount, ctx), 0
      if maxiters is not None and not isinstance(maxiters, int):
        raise Exception(f"Amount expression of for loop must evaluate to an integer")

      for value in srcseq.generator(ctx):
        value = execute(statement.body, { **ctx, statement.ident: value }, True, in_func)
        if value is BreakStmt: break
        numiters += 1
        if maxiters is not None and numiters >= maxiters: break

      continue

    # Function definitions and calls
    if isinstance(statement, FuncDefn):
      def create_user_func(fdef, def_ctx):
        def exec_user_func(call_ctx, *args):
          arity = len(fdef.argNames) if fdef.argNames is not None else 0
          if len(args) != arity: raise Exception(f"Function {fdef.ident} expects {arity} arguments, got {len(args)}")
          return execute(fdef.body, { **def_ctx, **{ fdef.argNames[i]: v for i,v in enumerate(args) } }, False, True)
        return exec_user_func
      ctx[statement.ident] = create_user_func(statement, ctx)
      continue

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

def exec_file(path, is_main=False, parent_ctx=None):
  with open(path, 'r') as file:  
    strin = decomment(file.read())
    parser = create_parser()

    ctx = mkctx(parent_ctx, {
      '$file':   os.path.realpath(path),
      '$dir':    os.path.dirname(os.path.realpath(path)),
      '$stdlib': os.path.realpath(os.path.dirname(os.path.realpath(__file__))+'/../stdlib'),
      '$main':   is_main
    })

    execute(parser.parse(strin), ctx)
    return ctx


# Utils

def mkctx(parent, add_vars={}):
  return {'$$parent':parent, **add_vars} if parent is not None else dict(add_vars)

def find_ident(ident, ctx):
  if ctx is None: return None
  default = default_ctx if ctx is not default_ctx else None

  parts = ident.split('~')
  debug = []

  for part in parts[:-1]:
    ctx = ctx[part] if part in ctx else find_ident(part, ctx.get('$$parent', default))
    debug.append(part)
    if ctx is None: 
      raise Exception(f"Can't get value from undefined namespace {'~'.join(debug)}")
    if not isinstance(ctx, dict):
      raise Exception(f"Can't get value of variable {'~'.join(debug)}; {part} is not a namespace")

  if parts[-1] in ctx: return ctx[parts[-1]]
  return find_ident(parts[-1], ctx.get('$$parent', default))