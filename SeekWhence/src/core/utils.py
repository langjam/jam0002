
def isnumber(x):
  return isinstance(x,int) or isinstance(x,float)

def istuple(x):
  return isinstance(x, tuple) and not isnamedtuple(x)

def isnamedtuple(x):
  return isinstance(x, tuple) and hasattr(x, '_asdict') and hasattr(x, '_fields')

def mktuple(*xs):
  return tuple(xs)

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