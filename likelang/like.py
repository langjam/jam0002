import sys

from lark import Lark
from lark.exceptions import UnexpectedCharacters, UnexpectedInput, VisitError

from like.eval import LikeEvaluator, LikeSyntaxError
from like.errors import syntax_error, runtime_error, get_context

filename = sys.argv[1]


with open("like.lark") as f:
    lark_parser = Lark(f.read(), start="start", parser="lalr")

with open(filename) as f:
    inp_str = f.read()
try:
    ast = lark_parser.parse(inp_str)
    res = LikeEvaluator().transform(ast)
except VisitError as ve:
    if isinstance(ve.orig_exc, LikeSyntaxError):
        runtime_error(str(ve.orig_exc))
    else:
        raise
except UnexpectedCharacters as u:
    print(get_context(u, inp_str))
    syntax_error(f"Unknown character at line {u.line}, column {u.column}")
except UnexpectedInput as u:
    print(get_context(u, inp_str))
    syntax_error(f"at line {u.line}, column {u.column}")
