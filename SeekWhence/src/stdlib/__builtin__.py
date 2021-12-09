
import sys
import math

from time import time as systime

from core.sequence import SequenceSlice, SequenceLike
from core.utils import isnumber

SEQ_EXPORTS = {
  'print':   lambda _,*s: print(*[printable(x) for x in s]),
  'input':   lambda _,p=None: (input() if p is None else input(p)) if sys.stdin.isatty() else sys.stdin.readline().rstrip('\n'),
  'systime': lambda _: math.floor(systime()*1000),

  'floor': lambda _,x: math.floor(x),
  'ceil':  lambda _,x: math.ceil(x),
  'round': lambda _,x: round(x),
  'abs':   lambda _,x: abs(x),
  'sign':  lambda _,x: 1 if x > 0 else -1 if x < 0 else 0,

  'unslice': lambda _,x: x.sequence if isinstance(x, SequenceSlice) else x,

  'isSequence': lambda _,x: isinstance(x, SequenceLike),
  'isSlice':    lambda _,x: isinstance(x, SequenceSlice),
  'isNumber':   lambda _,x: isnumber(x),
  'isString':   lambda _,x: isinstance(x, str),
  'isBoolean':  lambda _,x: isinstance(x, bool),
  'isNone':     lambda _,x: x is None,

  'true':  True,
  'false': False,
  'none':  None,
  'not':   lambda _,x: not x,

  'INF': math.inf,
  'NaN': math.nan
}

def printable(x):
  if x in [True,False,None]: return str(x).lower()
  if x in [math.inf,-math.inf]: return str(x).upper()
  if x in [math.nan]: return 'NaN'

  if isinstance(x, dict): 
    return f"[namespace {x['$$namespace_ident']} {{ {', '.join(n for n in list(x) if not n.startswith('$'))} }}]"

  return str(x)