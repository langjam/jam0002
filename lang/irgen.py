from nodes import *

class IRGen:
  def __init__(self, ast):
    self.tabs = 0
    self.out = ""
    self.consume(ast)
    print(self.out)

  def println(self, text):
    tabs = "  " * self.tabs
    return f"{tabs}{text}\n"

  def inctabs(self):
    self.tabs += 1

  def dectabs(self):
    self.tabs -= 1

  def consume(self, ast):
    if isinstance(ast, ProgramNode):
      meta = self.consume(ast.meta)
      cell = self.consume(ast.cell)
      aliases = self.consume(ast.aliases)
      selectors = self.consume(ast.selectors)
      rules = self.consume(ast.rules)
      self.out = meta + cell + aliases
    elif isinstance(ast, MetaNode):
      meta = self.println("meta = {")
      self.inctabs()
      for stmt in ast.stmts:
        meta += self.println(f"\"{stmt.name}\": {self.consume(stmt.exp)}")
      self.dectabs()
      meta += self.println("}")
      return meta
    elif isinstance(ast, CellNode):
      cell = self.println("cell = {")
      self.inctabs()
      for decl in ast.decls:
        cell += self.println(f"\"{decl.name}\": {self.consume(decl.exp)}")
      self.dectabs()
      cell += self.println("}")
      return cell
    elif isinstance(ast, AliasesNode):
      aliases = self.println("aliases = {")
      self.inctabs()
      for stmt_group in ast.stmt_groups:
        aliases += self.println(f"\"{stmt_group.name}\": {{")
        self.inctabs()
        for stmt in stmt_group.stmts:
          aliases += self.println(f"\"{stmt.name}\": {self.consume(stmt.exp)}")
        self.dectabs()
        aliases += self.println("}")
      self.dectabs()
      aliases += self.println("}")
      return aliases
    elif isinstance(ast, SelectorsNode):
      pass
    elif isinstance(ast, RulesNode):
      pass
    elif isinstance(ast, RuleStatementNode):
      pass
    elif isinstance(ast, StatementGroupNode):
      pass
    elif isinstance(ast, StatementNode):
      pass
    elif isinstance(ast, DeclarationNode):
      pass
    elif isinstance(ast, UnaryOpNode):
      pass
    elif isinstance(ast, BinOpNode):
      pass
    elif isinstance(ast, MatchCountNode):
      pass
    elif isinstance(ast, BoolLiteralNode):
      pass
    elif isinstance(ast, IntLiteralNode):
      pass
    elif isinstance(ast, ReferenceNode):
      pass
    else:
      raise ValueError(f"Unexpected node type {type(ast)}")
