from collections import namedtuple

# High level
ProgramNode = namedtuple('ProgramNode', ['meta', 'cell', 'aliases', 'selectors', 'rules'])
MetaNode = namedtuple('MetaNode', ['stmts'])
CellNode = namedtuple('CellNode', ['decls'])
AliasesNode = namedtuple('AliasesNode', ['stmt_groups'])
SelectorsNode = namedtuple('SelectorsNode', ['stmts'])
RulesNode = namedtuple('RulesNode', ['rule_stmts'])

# Declarations, statements, statement groups
RuleStatementNode = namedtuple('RuleStatementNode', ['ruleName', 'selectorName', 'stmts'])
StatementGroupNode = namedtuple('StatementGroupNode', ['name', 'stmts'])
StatementNode = namedtuple('StatementNode', ['name', 'exp'])
DeclarationNode = namedtuple('DeclarationNode', ['typ', 'name', 'exp'])

# Expression nodes
UnaryOpNode = namedtuple('UnaryOpNode', ['op', 'exp'])
BinOpNode = namedtuple('BinOpNode', ['op', 'left', 'right'])
MatchCountNode = namedtuple('MatchCountNode', ['dirs', 'selector'])
BoolLiteralNode = namedtuple('BoolLiteralNode', ['value'])
IntLiteralNode = namedtuple('IntLiteralNode', ['value'])
ReferenceNode = namedtuple('ReferenceNode', ['name'])
