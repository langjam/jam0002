from nodes import *
from direction_help import *
from collections import *

class Simulation:
    def __init__(self, ast):
        self.meta = {
            'rows': 20,
            'cols': 40,
        }
        self.aliases = {}
        self.cell_schema = None # populated once cell is processed
        self.selectors = None # populated once selectors are processed
        self.rules_by_selector = None # populated once rules are processed
        self.table = None # populated once meta and cell are processed
        self.consume(ast)


    def consume(self, node):
        if isinstance(node, ProgramNode):
            self.consume(node.meta)
            self.consume(node.cell)
            self.consume(node.aliases)
            self.consume(node.selectors)
            self.consume(node.rules)
        elif isinstance(node, MetaNode):
            for stmt in node.stmts:
                self.meta[stmt.name] = self.evaluate(stmt.exp)
        elif isinstance(node, CellNode):
            cell_props = {}
            for decl in node.decls:
                cell_props[decl.name] = self.evaluate(decl.exp)
            self.cell_schema = namedtuple("Cell", ['row', 'col'] + cell_props.keys(), defaults=[None, None] + cell_props.values())
            # Now that meta and cell are defined, put a default cell in every slot in the table
            self.table = [[self.cell_schema() for x in range(self.meta['cols'])] for y in range(self.meta['rows'])]
        elif isinstance(node, AliasesNode):
            for stmt_group in node.stmt_groups:
                alias_name = stmt_group.name
                alias_props = {}
                for stmt in stmt_group.stmts:
                    alias_props[stmt.name] = self.evaluate(stmt.exp)
                self.aliases[alias_name] = self.cell_schema()._replace(**alias_props)
        elif isinstance(node, SelectorsNode):
            # TODO something needs to make sure selector names don't conflict with cell attributes
            self.selectors = dict()
            for stmt in node.stmts:
                # Save expressions as-is for evaluation later on
                self.selectors[stmt.name] = stmt.exp
        elif isinstance(node, RulesNode):
            # Store rules by selector for quick lookups
            self.rules_by_selector = dict()
            for rule in node.rule_stmts:
                if hasattr(self.rules_by_selector, rule.selectorName):
                    raise Exception(f"Only one rule can exist for every selector")
                self.rules_by_selector[rule.selectorName] = rule.stmts
        else:
            raise ValueError(f"Unexpected node type {type(node)}")


    def evaluate(self, exp, cell=None, is_static=True):
        if isinstance(exp, UnaryOpNode):
            if exp.op == "MINUS":
                return -self.evaluate(exp.exp, cell, is_static)
            elif exp.op == "NOT":
                return not self.evaluate(exp.exp, cell, is_static)
            raise ValueError(f"Unexpected unary op node: {type(exp)}")
        elif isinstance(exp, BinOpNode):
            if exp.op == "+":
                return self.evaluate(exp.left, cell, is_static) + self.evaluate(exp.right, cell, is_static)
            elif exp.op == "-":
                return self.evaluate(exp.left, cell, is_static) - self.evaluate(exp.right, cell, is_static)
            elif exp.op == "*":
                return self.evaluate(exp.left, cell, is_static) * self.evaluate(exp.right, cell, is_static)
            elif exp.op == "/":
                return self.evaluate(exp.left, cell, is_static) // self.evaluate(exp.right, cell, is_static)
            elif exp.op == "%":
                return self.evaluate(exp.left, cell, is_static) % self.evaluate(exp.right, cell, is_static)
            raise ValueError(f"Unexpected binary op node: {exp}")
        elif isinstance(exp, BoolLiteralNode) or isinstance(exp, IntLiteralNode):
            return exp.value
        elif isinstance(exp, MatchCountNode):
            if is_static:
                raise Exception("Can't evaluate MatchCount in static context")
            # get targets, run selector on all targets, return count of matches
            matches = 0
            for n in exp.dirs:
                row_offset, col_offset = direction_offsets[n]
                if self.evaluate_at(exp.selector, cell.row + row_offset, cell.col + col_offset):
                    n += 1
            pass
        elif isinstance(exp, ReferenceNode):
            if is_static:
                raise Exception("Can't resolve reference in static context")
            # name will refer to either a Selector, or a Cell attribute
            if exp.name in self.selectors:
                # evaluate the selector
                return self.evaluate(self.selectors[exp.name], cell, is_static=False)
            elif hasattr(cell, exp.name):
                # resolve the variable from the local cell
                return getattr(cell, exp.name)
            else:
                raise NameError(f"Can't resolve reference '{exp.name}'")
        else:
            raise ValueError(f"Unexpected node type for evaluator: {type(exp)}")

    def evaluate_at(self, exp, y, x):
        cell_copy = self.table[y][x]._replace()
        cell_copy.row = y
        cell_copy.col = x
        return self.evaluate(exp, cell=cell_copy, is_static=False)

    # user input has been set and the simulation should now run
    def run(self):
        pass

    # Run over the table and apply rules to cells
    def tick(self):
        pass
