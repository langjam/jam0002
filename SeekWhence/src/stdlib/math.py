import math

SEQ_EXPORTS = {
	'sqrt':  lambda _,x: math.sqrt(x),
  'log':   lambda _,x,b=None: math.log(x,b) if b is not None else math.log(x),
  'sin':   lambda _,x: math.sin(x),
  'cos':   lambda _,x: math.cos(x),
  'tan':   lambda _,x: math.tan(x),
  'asin':  lambda _,x: math.asin(x),
  'acos':  lambda _,x: math.acos(x),
  'atan':  lambda _,x: math.atan(x),
  'atan2': lambda _,x,y: math.atan2(y,x),
  'toDeg': lambda _,x: math.degrees(x),
  'toRad': lambda _,x: math.radians(x),
  
  'PI':    math.pi,
  'E':     math.e,
  'TAU':   math.tau,
  'PHI':   (1 + 5 ** 0.5) / 2
}