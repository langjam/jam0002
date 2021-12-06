# High level
class ProgramNode:
    def __init__(self, meta, cell, aliases, selectors, rules):
        self.meta = meta
        self.cell = cell
        self.aliases = aliases
        self.selectors = selectors
        self.rules = rules

class MetaNode:
    def __init__(self, stmts):
        self.stmts = stmts

class CellNode:
    def __init__(self, declarations):
        self.declarations = declarations

class AliasesNode:
    def __init__(self, stmt_groups):
        self.stmt_groups = stmt_groups

class SelectorsNode:
    def __init__(self, stmts):
        self.stmts = stmts

class RulesNode:
    def __init__(self, rule_stmts):
        self.rule_stmts = rule_stmts

# Declarations, statements, statement groups
class RuleStatementNode:
    def __init__(self, ruleName, selectorName, stmts):
        self.ruleName = ruleName
        self.selectorName = selectorName
        self.stmts = stmts

class StatementGroupNode:
    def __init__(self, name, stmts):
        self.name = name
        self.stmts = stmts

class StatementNode:
    def __init__(self, name, exp):
        self.name = name
        self.exp = exp

class DeclarationNode:
    def __init__(self, typ, name, exp):
        self.typ = typ
        self.name = name
        self.exp = exp

# Expression nodes
class BinOpNode:
    def __init__(self, left, right):
        self.left = left
        self.right = right

class PlusNode(BinOpNode):
    pass

class MinusNode(BinOpNode):
    pass

class MultiplyNode(BinOpNode):
    pass

class DivideNode(BinOpNode):
    pass

class GreaterNode(BinOpNode):
    pass

class GreaterEq(BinOpNode):
    pass

class LessNode(BinOpNode):
    pass

class LessEqNode(BinOpNode):
    pass

class EqualsNode(BinOpNode):
    pass

class OrNode(BinOpNode):
    pass

class AndNode(BinOpNode):
    pass

class MatchCountNode:
    def __init__(self, lst, selector):
        self.lst = lst
        self.selector= selector

class BoolLiteralNode:
    def __init__(self, value):
        self.value = value

class IntLiteralNode:
    def __init__(self, value):
        self.value = value

class VariableNode:
    def __init__(self, name):
        self.name = name

class UnaryOpNode:
    def __init__(self, exp):
        self.exp = exp

class NotNode(UnaryOpNode):
    pass

class UMinusNode(UnaryOpNode):
    pass

# Directions
class DirectionNode:
    def __init__(self, deer):
        self.deer = deer

class DirectionListNode:
    def __init__(self, directions):
        self.directions = directions
