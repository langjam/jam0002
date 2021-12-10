
from abc import ABC, abstractmethod
from dataclasses import dataclass, replace as copy_dc

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

  def __post_init__(self):
    from .expr import simplify_expr
    self.__cache = {}
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
    from .expr import exec_expr, find_in_expr

    if n < 0: return 0
    if n in self.__cache: return self.__cache[n]

    if self.baseCases is not None and n < len(self.baseCases):
      self.__cache[n] = exec_expr(self.baseCases[n], ctx)
    else:
      expn = (n-len(self.baseCases)) % len(self.exprs)
      expr = self.exprs[expn]

      if prev is None:
        if n <= 0: prev = 0
        elif n-1 in self.__cache: prev = self.__cache[n-1]

        # Load previous values into the cache in a flat loop, instead of deep recursion
        # This is only needed when the previous value isn't passed or already cached,
        #   and when x or S are actually found in the expression.
        elif find_in_expr(expr, 'x', 'S'):
          maxi = max(i for i in [*self.__cache.keys(),0])
          prev = self.value_at(maxi, ctx)
          for cn in range(maxi,n):
            self.__cache[cn] = prev = self.value_at(cn, ctx, prev)

      self.__cache[n] = exec_expr(expr, { **ctx, 'x': prev, 'n': n, 'm': expn, 'S': self })

    return self.__cache[n]

  def __str__(self):
    from .expr import display_expr
    exprs = ", ".join([display_expr(e) for e in self.exprs])
    if len(self.baseCases) > 0:
      basec = ", ".join([display_expr(i) for i in self.baseCases])
      return f"[sequence {self.ident}: {basec} | {exprs}]"
    return f"[sequence {self.ident}: {exprs}]"

  @staticmethod
  def build(ident='<anonymous>', exprs=None, baseCases=None, sliceIndx=None):
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
    from .expr import display_expr

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
    from .expr import expr_equal

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