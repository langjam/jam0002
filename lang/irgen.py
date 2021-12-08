from nodes import *

class IRGen:
  def __init__(self, ast):
    self.tabs = 0
    self.out = ""
    self.cell_props = set()
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
      preamble = self.println("from collections import namedtuple")
      preamble += self.println("Rule = namedtuple(\"Rule\", [\"name\", \"selector\", \"func\"])")
      meta = self.consume(ast.meta)
      cell = self.consume(ast.cell)
      aliases = self.consume(ast.aliases)
      selectors = self.consume(ast.selectors)
      rules = self.consume(ast.rules)
      self.out = preamble + meta + cell + aliases + selectors + rules
    elif isinstance(ast, MetaNode):
      meta = self.println("meta = {")
      self.inctabs()
      for stmt in ast.stmts:
        meta += self.println(f"\"{stmt.name}\": {self.consume(stmt.exp)},")
      self.dectabs()
      meta += self.println("}")
      return meta
    elif isinstance(ast, CellNode):
      cell = self.println("cell = {")
      self.inctabs()
      for decl in ast.decls:
        self.cell_props.add(decl.name)
        cell += self.println(f"\"{decl.name}\": {self.consume(decl.exp)},")
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
          aliases += self.println(f"\"{stmt.name}\": {self.consume(stmt.exp)},")
        self.dectabs()
        aliases += self.println("},")
      self.dectabs()
      aliases += self.println("}")
      return aliases
    elif isinstance(ast, SelectorsNode):
      selectors = self.println("selectors = {")
      self.inctabs()
      for stmt in ast.stmts:
        func = f"lambda selectors, cell, builtins: {self.consume(stmt.exp)}"
        selectors += self.println(f"\"{stmt.name}\": {func},")
      self.dectabs()
      selectors += self.println("}")
      return selectors
    elif isinstance(ast, RulesNode):
      rules = self.println("rules = [")
      self.inctabs()
      for rule_stmt in ast.rule_stmts:
        rules += self.consume(rule_stmt)
      self.dectabs()
      rules += self.println("]")
      return rules
    elif isinstance(ast, RuleStatementNode):
      func = "lambda cell: {"
      for stmt in ast.stmts:
        func += f"\"{stmt.name}\": {self.consume(stmt.exp)}"
      func += "}"
      rule = self.println(f"Rule(name=\"{ast.ruleName}\", selector=\"{ast.selectorName}\", func={func}),")
      return rule
    elif isinstance(ast, StatementGroupNode):
      pass
    elif isinstance(ast, StatementNode):
      pass
    elif isinstance(ast, DeclarationNode):
      pass
    elif isinstance(ast, UnaryOpNode):
      if ast.op == "-":
        return f"-({self.consume(ast.exp)})"
      elif ast.op == "!":
        return f"not ({self.consume(ast.exp)})"
    elif isinstance(ast, BinOpNode):
      if ast.op == "+":
        return f"({self.consume(ast.left)}) + ({self.consume(ast.right)})"
      elif ast.op == "-":
        return f"({self.consume(ast.left)}) - ({self.consume(ast.right)})"
      elif ast.op == "*":
        return f"({self.consume(ast.left)}) * ({self.consume(ast.right)})"
      elif ast.op == "/":
        return f"({self.consume(ast.left)}) // ({self.consume(ast.right)})"
      elif ast.op == "%":
        return f"({self.consume(ast.left)}) % ({self.consume(ast.right)})"
      elif ast.op == "&":
        return f"({self.consume(ast.left)}) and ({self.consume(ast.right)})"
      elif ast.op == "|":
        return f"({self.consume(ast.left)}) or ({self.consume(ast.right)})"
      elif ast.op == ">":
        return f"({self.consume(ast.left)}) > ({self.consume(ast.right)})"
      elif ast.op == "<":
        return f"({self.consume(ast.left)}) < ({self.consume(ast.right)})"
      elif ast.op == ">=":
        return f"({self.consume(ast.left)}) >= ({self.consume(ast.right)})"
      elif ast.op == "<=":
        return f"({self.consume(ast.left)}) <= ({self.consume(ast.right)})"
      elif ast.op == "==":
        return f"({self.consume(ast.left)}) == ({self.consume(ast.right)})"
    elif isinstance(ast, MatchCountNode):
      return f"builtins[\"matchcount\"]({ast.dirs}, selectors[\"{ast.selector}\"], cell)"
    elif isinstance(ast, BoolLiteralNode):
      if ast.value:
        return "True"
      return "False"
    elif isinstance(ast, IntLiteralNode):
      return f"{ast.value}"
    elif isinstance(ast, ReferenceNode):
      if ast.name in self.cell_props:
        return f"cell.{ast.name}"
      else:
        return f"selectors[\"{ast.name}\"](selectors, cell, builtins)"
    else:
      print(ast)
      raise ValueError(f"Unexpected node type {type(ast)}")
