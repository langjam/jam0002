from typing import Any, Dict, List, Tuple, cast

from lark import Transformer
from lark.lexer import Token
from lark.tree import Tree
from lark.visitors import v_args

from like.types import Variable, Collect, Function


class LikeSyntaxError(Exception):
    pass

class LikeEvaluator(Transformer):
    def __init__(self):
        self._scopes: List[Dict[str, Any]] = [{}]
        self._func_counter: int = 0

    def _should_not_eval(self) -> bool:
        return self._func_counter > 0

    @v_args(tree=True)
    def expression(self, tree: Tree):
        if self._should_not_eval():
            return tree

        items = cast(List, tree.children)

        if len(items) == 3:
            op1, operator, op2 = items
            operator: Tree
            if isinstance(op1, Variable):
                op1 = op1.value
            if isinstance(op2, Variable):
                op2 = op2.value
            if operator.data == "add":
                return op1 + op2
            elif operator.data == "sub":
                return op1 - op2
            elif operator.data == "mul":
                return op1 * op2
            elif operator.data == "div":
                return op1 / op2
            else:
                raise LikeSyntaxError("aaaa unknown operator")

        elif len(items) == 1:
            return items[0]

        else:
            raise LikeSyntaxError(
                f"Expression did not get 3 things what to dooo: {items}"
            )

    @v_args(tree=True)
    def number(self, tree: Tree):
        if self._should_not_eval():
            return tree

        return float(cast(str, tree.children)[0])

    @v_args(tree=True)
    def string(self, tree: Tree):
        if self._should_not_eval():
            return tree
        return cast(str, tree.children)[0].strip('"')

    @v_args(tree=True)
    def assignment(self, tree: Tree):
        if self._should_not_eval():
            return tree

        identifier, res = cast(List, tree.children)
        var = Variable(identifier, res)
        self._scopes[-1][identifier] = var
        return var

    @v_args(tree=True)
    def identifier(self, tree: Tree):
        if self._should_not_eval():
            return tree

        return cast(Token, tree.children[0]).value

    @v_args(tree=True)
    def existing_ident(self, tree: Tree):
        if self._should_not_eval():
            return tree

        items = cast(List, tree.children)
        ident = self._get_ident(items[0])
        if not ident:
            raise LikeSyntaxError(f"Unknown variable: {items[0]}")
        return ident

    @v_args(tree=True)
    def start_fn(self, tree: Tree):
        self._func_counter += 1

        return tree if self._should_not_eval() else None

    @v_args(tree=True)
    def end_fn(self, tree: Tree):
        self._func_counter -= 1

        return tree if self._should_not_eval() else None

    @v_args(tree=True)
    def function(self, tree: Tree):
        if self._should_not_eval():
            return tree

        name, args, _, body, _ = cast(List, tree.children)
        func = Function(name, args, body)
        self._scopes[-1][name] = func
        return func

    @v_args(tree=True)
    def func_call(self, tree: Tree):
        if self._should_not_eval():
            return tree

        name, args = cast(List, tree.children)
        if name == "print":
            args = list(
                map(
                    lambda x: "false"
                    if isinstance(x, Variable) and x.value is False
                    else x,
                    map(
                        lambda x: "true"
                        if isinstance(x, Variable) and x.value is True
                        else x,
                        args,
                    ),
                )
            )
            print(*args)
            return None

        if name == "input":
            if len(args) != 0:
                raise LikeSyntaxError("input expects no args!")
            inp = input()
            return inp

        ident_scope = self._get_ident(name)
        if not ident_scope:
            raise LikeSyntaxError("function not found")
        elif isinstance(ident_scope, Variable):
            if isinstance(ident_scope.value, Function):
                ident_scope = ident_scope.value
            else:
                raise LikeSyntaxError("tried to call variable")
        elif isinstance(ident_scope, Collect):
            funs = [fun for name, fun in ident_scope.value if name == ""]

            if not funs:
                raise LikeSyntaxError("Function not found")
            if len(funs) > 1:
                raise LikeSyntaxError("Too many functions found aaa")

            ident_scope = funs[0]
        elif len(ident_scope.args) != len(args):
            raise LikeSyntaxError(
                "expected: {} args, got {}.".format(len(ident_scope.args), len(args))
            )
        self._scopes.append(
            {
                param: arg
                if isinstance(arg, Variable)
                else Variable(param, arg)
                for param, arg in zip(ident_scope.args, args)
            }
        )
        result = self.transform(ident_scope.value)
        self._scopes.pop()
        return result

    @v_args(tree=True)
    def params(self, tree: Tree):
        return tree if self._should_not_eval() else tree.children

    @v_args(tree=True)
    def args(self, tree: Tree):
        return tree if self._should_not_eval() else tree.children

    @v_args(tree=True)
    def block(self, tree: Tree):
        if self._should_not_eval():
            return tree

        return tree.children[-1]

    @v_args(tree=True)
    def start(self, tree: Tree):
        if self._should_not_eval():
            return tree

        return tree.children[-1]

    def _get_ident(self, ident: str):
        for scope in reversed(self._scopes):
            if ident in scope.keys():
                return scope[ident]

    @v_args(tree=True)
    def collect(self, tree: Tree):
        if self._should_not_eval():
            return tree

        identifier, pattern = cast(List, tree.children)
        pattern = pattern.strip("/")
        functions = self._get_functions()

        if pattern[-1] == "*":
            pattern_type = "prefix"

            def filter_func(fun):
                return fun[0].startswith(pattern[:-1])

            def extract_func(fun):
                return fun[0][len(pattern[:-1]) :]

        elif pattern[0] == "*":
            pattern_type = "postfix"

            def filter_func(fun):
                return fun[0].endswith(pattern[1:])

            def extract_func(fun):
                return fun[0][: -len(pattern[1:])]

        else:
            raise LikeSyntaxError("Invalid pattern. Use a * at the beginning or end.")

        matching_functions = list(filter(filter_func, functions))
        fun_names = list(map(extract_func, matching_functions))
        _collect = Collect(
            [
                (fun_name, fun)
                for fun_name, (_, fun) in zip(fun_names, matching_functions)
            ],
            pattern_type,
        )
        self._scopes[-1][identifier] = _collect
        return _collect

    def _get_functions(self) -> List[Tuple[str, Any]]:
        function = []
        for scope in self._scopes:
            for (key, value) in scope.items():
                if isinstance(value, Function):
                    function.append((key, value))
        return function

    @v_args(tree=True)
    def collect_call(self, tree: Tree):
        # do not evaluate until execution is reached
        if self._should_not_eval():
            return tree

        # destructuring the tree
        prefix, postfix, args = cast(List, tree.children)

        # checking if prefix exists
        pref_scope = self._get_ident(prefix)
        if isinstance(pref_scope, Collect):
            # getting functions that match the postfix
            matching_function = list(
                filter(lambda x: x[0] == postfix, cast(List[Tuple], pref_scope.value))
            )

            # checking if no funtions match
            if len(matching_function) < 1:
                raise LikeSyntaxError("Tried to call invalid function")

            # checking if more than function matches
            if len(matching_function) > 1:
                raise LikeSyntaxError("Ambiguous function call")

            # storing the matching function
            function = matching_function[0][1]
        else:
            # check if postfix exists
            post_scope = self._get_ident(postfix)
            if isinstance(post_scope, Collect):
                # getting functions that match the prefix
                matching_function = list(
                    filter(
                        lambda x: x[0] == prefix, cast(List[Tuple], post_scope.value)
                    )
                )

                # checking if no functions match
                if len(matching_function) < 1:
                    raise LikeSyntaxError("Tried to call invalid function")

                # checking if multiple functions match
                if len(matching_function) > 1:
                    raise LikeSyntaxError("Ambiguous function call")

                # storing the matching function
                function = matching_function[0][1]
            else:
                raise LikeSyntaxError("Trying to use collect on no collect types")

        # check if args match
        if len(function.args) != len(args):
            raise LikeSyntaxError(
                "expected: {} args, got {}.".format(len(function.args), len(args))
            )

        # create a new scope with args
        self._scopes.append(
            {
                param: Variable(param, arg)
                for param, arg in zip(function.args, args)
            }
        )

        # get the result of the function and pop the scope
        result = self.transform(function.value)
        self._scopes.pop()

        return result

    @v_args(tree=True)
    def true(self, tree: Tree):
        # do not evaluate until execution is reached
        if self._should_not_eval():
            return tree
        return True

    @v_args(tree=True)
    def false(self, tree: Tree):
        # do not evaluate until execution is reached
        if self._should_not_eval():
            return tree
        return False

    @v_args(tree=True)
    def conditional(self, tree: Tree):
        # do not evaluate until execution is reached
        if self._should_not_eval():
            return tree

        items = cast(List, tree.children)

        if len(items) == 1:
            return items[0]
        if len(items) == 2:
            return not items[1]
        if len(items) == 3:
            op1, operator, op2 = items
            operator: Tree
            if isinstance(op1, Variable):
                op1 = op1.value
            if isinstance(op2, Variable):
                op2 = op2.value

            if operator.data == "eq":
                return op1 == op2
            if operator.data == "lt":
                return op1 < op2
            if operator.data == "gt":
                return op1 > op2
            if operator.data == "lte":
                return op1 <= op2
            if operator.data == "gte":
                return op1 >= op2
            else:
                raise LikeSyntaxError(f"Unknown operator: {operator.data}")

        raise LikeSyntaxError("Invalid conditional")

    @v_args(tree=True)
    def control_statement(self, tree: Tree):
        # do not evaluate until execution is reached
        if self._should_not_eval():
            return tree

        items = cast(List, tree.children)

        if_cond, _, if_block, _ = items[:4]

        if if_cond:
            return self.transform(if_block)

        if len(items) < 5:
            return

        next_ind = 4
        while len(items) > next_ind + 4:
            next_ = items[next_ind]
            if next_ is not None:
                # else if block

                if_cond, _, if_block, _ = items[next_ind : next_ind + 4]
                next_ind += 4
                if if_cond:
                    return self.transform(if_block)
            else:
                break

        if len(items) > next_ind:
            _, else_block, _ = items[next_ind:]

            if else_block is None:
                return

            return self.transform(else_block)

        raise LikeSyntaxError("Control statement go brr")
