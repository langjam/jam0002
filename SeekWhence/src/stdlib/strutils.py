
from core.sequence import Sequence

SEQ_EXPORTS = {
	'length': lambda _,s: len(s),

	'startsWith': lambda _,h,n: h.startswith(n),
	'endsWith':   lambda _,h,n: h.endswith(n),
	'indexOf':    lambda _,h,n: r if (r := h.find(n)) > -1 else None,

	'split': lambda _,h,n: Sequence.build(ident = '<split>', exprs = [None, *h.split(n)], sliceIndx = 1),
	'toSeq': lambda _,s: Sequence.build(ident = '<toSeq>', exprs = [None, *s], sliceIndx = 1),

	'trim':      lambda _,s: s.strip(),
	'trimLeft':  lambda _,s: s.lstrip(),
	'trimRight': lambda _,s: s.rstrip(),
}
