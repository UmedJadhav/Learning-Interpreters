'''
  Language Grammar :
  ---------------------
  program : PROGRAM variable SEMI block DOT
  block: declaration compund_statements
  declarations: VAR(variable_decalration SEMI)+ | (PROCEDURE ID SEMI block SEMI)* | empty
  variable_declaration: ID(COMMA ID)* COLON type_spec
  type_spec : INTEGER |  REAL
  compound_statement : BEGIN statement_list END
  statement_list : statement | statement SEMI statement_list
  statement : compund_statement | assignment_statement | empty
  assignment_statement : variable ASSIGN expr
  empty:
  expr : term((PLUS | MINUS) term)*
  term : factor((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
  factor : PLUS factor | MINUS factor | INTEGER_CONST | REAL_CONST | LPAREN expr RPAREN | variable
  variable : ID
'''
from collections import OrderedDict

class Token:
  def __init__(self, _type, value):
    self._type = _type
    self.value = value

  def __str__(self):
    return f"Token({self._type}, {self.value})"
  
  def __repr__(self):
    return self.__str__()

RESERVED_KEYWORDS = {
  'BEGIN' : Token('BEGIN','BEGIN'),
  'END': Token('END','END'),
  'PROGRAM': Token('PROGRAM', 'PROGRAM'),
  'VAR' : Token('VAR', 'VAR'),
  'DIV': Token('INTEGER_DIV', 'DIV'),
  'INTEGER': Token('INTEGER', 'INTEGER'),
  'REAL': Token('REAL', 'REAL'),
  'PROCEDURE' : Token('PROCEDURE', 'PROCEDURE')
}

class Symbol(object):
  def __init__(self, name, type=None):
    self.name = name
    self.type = type

class BuiltinTypeSymbol(Symbol):
  def __init__(self, name):
    super().__init__(name)
  
  def __str__(self):
    return self.name
  
  def __repr__(self):
    return self.__str__()

class VarSymbol(Symbol):
  def __init__(self, name, type):
    super().__init__(name, type)
  
  def __str__(self):
    return f'<{self.name} : {self.type}>'
  
  def __repr__(self):
    return self.__str__()

class SymbolTable:
  def __init__(self):
    self._symbols = OrderedDict()
    self.__init_buildins()
  
  def __init_buildins(self):
    self.define(BuiltinTypeSymbol('INTEGER'))
    self.define(BuiltinTypeSymbol('REAL'))
  
  def __str__(self):
    s = f'Symbols: {[value for value in self._symbols.values()]}'
    return s

  def __repr__(self):
    return self.__str__()
  
  def define(self, symbol):
    print(f'Define: {symbol}')
    self._symbols[symbol.name] = symbol
  
  def lookup(self, name):
    print(f'Lookup: {name}')
    symbol = self._symbols.get(name)
    return symbol

class Lexer:
  # Breaks sentences into tokens
  def __init__(self, text):
    self.text = text
    self.pos = 0 # index into self.text
    self.current_char = self.text[self.pos]

  def error(self):
    raise Exception('Invalid Character')

  def _id(self):
    # Used to handle variable_names and reserved Keywords
    result = ''
    while self.current_char is not None and self.current_char.isalnum():
      result += self.current_char
      self.advance()
    
    token = RESERVED_KEYWORDS.get(result, Token('ID', result))
    return token
  
  def skip_comment(self):
    while self.current_char != '}':
      self.advance()
    self.advance() # To eat up the closing brace
  
  def number(self):
    # Returns a multidigit integer or float consumed from the input
    result = ''
    while self.current_char is not None and self.current_char.isdigit():
      result += self.current_char
      self.advance()
    
    if self.current_char == '.':
      result += self.current_char
      self.advance()

      while (self.current_char is not None and self.current_char.isdigit()):
        result += self.current_char
        self.advance()
      
      token = Token('REAL_CONST', float(result))
    else:
      token = Token('INTEGER_CONST', int(result))
    
    return token

  def peek(self):
    # Used to peek ahead to determine distinguish bw symbols like `:` amd `:=` etc
    peek_pos = self.pos + 1
    if peek_pos > len(self.text) - 1:
      return None
    else:
      return self.text[peek_pos]

  def advance(self):
    self.pos += 1
    if self.pos > len(self.text) - 1:
      self.current_char = None # Indicates the end of input
    else:
      self.current_char = self.text[self.pos]

  def skip_whitespace(self):
    while self.current_char is not None and self.current_char.isspace():
      self.advance()

  def integer(self):
    # Returns a multidigit integer consumed from input
    result = ''
    while self.current_char is not None and self.current_char.isdigit():
      result += self.current_char
      self.advance()
    return int(result)

  def get_next_token(self):
    # Lexical Analyzer
    while self.current_char is not None:
      
      if self.current_char.isspace():
        self.skip_whitespace()
        continue

      if self.current_char ==  '+':
        self.advance()
        return  Token('PLUS','+')

      if self.current_char ==  '-':
        self.advance()
        return  Token('MINUS','-')  

      if self.current_char == '*':
        self.advance()
        return Token('MUL','*')

      if self.current_char == '/':
        self.advance()
        return Token('FLOAT_DIV','/')    
    
      if self.current_char == '(' :
        self.advance()
        return Token('LPAREN','(')

      if self.current_char == ')' :
        self.advance()
        return Token('RPAREN',')')
      
      if self.current_char.isalpha():
        return self._id()
      
      if self.current_char == ':' and self.peek() == '=':
        self.advance()
        self.advance()
        return Token('ASSIGN', ':=')
      
      if self.current_char == ';':
        self.advance()
        return Token('SEMI', ';')
      
      if self.current_char == '.':
        self.advance()
        return Token('DOT', '.')
      
      if self.current_char == '{' :
        self.advance()
        self.skip_comment()
        continue
      
      if self.current_char.isdigit():
        return self.number()
      
      if self.current_char == ':':
        self.advance()
        return Token('COLON', ':')
      
      if self.current_char == ',':
        self.advance()
        return Token('COMMA', ',')
      
      if self.current_char == '/':
        self.advance()
        return Token('FLOAT_DIV', '/')

      self.error() # If the token is not any of the recognized one , raise an error

    return Token('EOF',None)

class AST:
  pass

class Program(AST):
  def __init__(self, name, block):
    self.name = name
    self.block = block

class Block(AST):
  def __init__(self, declarations , compound_statement):
    self.declarations = declarations
    self.compound_statement = compound_statement

class VarDecl(AST):
  def __init__(self, var_node, type_node):
    self.var_node = var_node
    self.type_node = type_node

class Type(AST):
  def __init__(self, token):
    self.token = token
    self.value = token.value

class BinOp(AST):
  # Binary operators operate on 2 operands
  def __init__(self, left, op, right):
    self.left = left
    self.token = self.op = op
    self.right = right
  
  def __str__(self):
    return f"BinOp : {self.left} {self.token} {self.right}"

class UnaryOp(AST):
  def __init__(self, op, expr):
    self.token = self.op = op
    self.expr = expr
  
  def __str__(self):
    return f"Unary : {self.token} {self.expr}"
class Num(AST):
  def __init__(self, token):
    self.token = token
    self.value= token.value
  
  def __str__(self):
    return f"Num : {self.token} {self.value}"

class Compound(AST):
  # Represents a BEGIN..END block
  def __init__(self):
    self.children = []
  
  def __str__(self):
    return f"Compound: {self.children}"

class Assign(AST):
  def __init__(self, left, op, right):
    self.left = left
    self.token = self.op = op
    self.right = right

  def __str__(self):
    return f"Assign: {self.left} {self.token} {self.right}"

class Var(AST):
  # Var is build from ID token
  def __init__(self, token):
    self.token = token
    self.value = token.value
  
  def __str__(self):
    return f"Var: {self.token} {self.value}"

class NoOp(AST):
  # Used to represent empty statement
  pass

class ProcedureDecl(AST):
  def __init__(self, proc_name, block_node):
    self.proc_name = proc_name
    self.block_node = block_node

class Parser:
  # Recognizes structure in a stream of token
  def __init__(self, lexer):
    self.lexer = lexer
    self.current_token = self.lexer.get_next_token()
  
  def error(self):
    raise Exception('Invalid Syntax')

  def eat(self, token_type):
    if self.current_token._type == token_type :
      self.current_token = self.lexer.get_next_token()
    else:
      self.error()
  
  def program(self):
    # program : PROGRAM variable SEMI block DOT
    self.eat('PROGRAM')
    var_node = self.variable()
    prog_name = var_node.value
    self.eat('SEMI')
    block_node = self.block()
    program_node = Program(prog_name, block_node)
    self.eat('DOT')
    return program_node
  
  def block(self):
    # block : declarations compound_statement
    declaration_node = self.declarations()
    compound_statement_node = self.compound_statement()
    node = Block(declaration_node, compound_statement_node)
    return node
  
  def declarations(self):
    # declarations: VAR(variable_decalration SEMI)+ | (PROCEDURE ID SEMI block SEMI)* | empty
    declarations = []
    if self.current_token._type == 'VAR':
      self.eat('VAR')
      while self.current_token._type == 'ID':
        var_decl = self.variable_declaration()
        declarations.extend(var_decl)
        self.eat('SEMI')
    
    while self.current_token._type == 'PROCEDURE':
      self.eat('PROCEDURE')
      proc_name = self.current_token.value
      self.eat('ID')
      self.eat('SEMI')
      block_node = self.block()
      proc_decl = ProcedureDecl(proc_name, block_node)
      declarations.append(proc_decl)
      self.eat('SEMI')

    return declarations
  
  def variable_declaration(self):
    # variable_declaration: ID(COMMA ID)* COLON type_spec
    var_nodes = [Var(self.current_token)]
    self.eat('ID')

    while self.current_token._type == 'COMMA':
      self.eat('COMMA')
      var_nodes.append(Var(self.current_token))
      self.eat('ID')
    
    self.eat('COLON')
    type_node = self.type_spec()
    var_declarations = [ VarDecl(var_node, type_node) for var_node in var_nodes ]
    return var_declarations
  
  def type_spec(self):
    # type_spec : INTEGER |  REAL
    token = self.current_token
    if self.current_token._type == 'INTEGER':
      self.eat('INTEGER')
    else:
      self.eat('REAL')
    
    node = Type(token)
    return node
  
  def compound_statement(self):
    # compound_statement : BEGIN statement_list END
    self.eat('BEGIN')
    nodes = self.statement_list()
    self.eat('END')
    root = Compound()
    for node in nodes:
      root.children.append(node)
    return root
  
  def statement_list(self):
    # statement_list : statement | statement SEMI statement_list
    node = self.statement()
    results = [node]

    while self.current_token._type == 'SEMI':
      self.eat('SEMI')
      results.append(self.statement())

    return results

  def statement(self):
    # statement : compound_statement | assignment_statement | empty
    if self.current_token._type == 'BEGIN':
      node = self.compound_statement()
    elif self.current_token._type == 'ID':
      node = self.assignment_statement()
    else:
      node = self.empty()
    return node

  def assignment_statement(self):
    # assignment_statement : variable ASSIGN expr
    left = self.variable()
    token = self.current_token
    self.eat('ASSIGN')
    right = self.expr()
    node = Assign(left, token, right)
    return node
  
  def variable(self):
    # variable: ID
    node = Var(self.current_token)
    self.eat('ID')
    return node
  
  def empty(self):
    return NoOp()

  def factor(self):
    # factor : PLUS factor | MINUS factor | INTEGER_CONST | REAL_CONST | LPAREN expr RPAREN | variable
    
    token = self.current_token
    if token._type == 'PLUS':
      self.eat('PLUS')
      node = UnaryOp(token, self.factor())
      return node
    elif token._type == 'MINUS':
      self.eat('MINUS')
      node = UnaryOp(token, self.factor())
      return node
    elif token._type == 'INTEGER_CONST':
      self.eat('INTEGER_CONST')
      return Num(token)
    elif token._type == 'REAL_CONST':
      self.eat('REAL_CONST')
      return Num(token)
    elif token._type == 'LPAREN':
      self.eat('LPAREN')
      node = self.expr()
      self.eat('RPAREN')
      return node
    else:
      node = self.variable()
      return node
  
  def term(self):
    # term: factor((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
    node = self.factor()
    while self.current_token._type in ('MUL', 'INTEGER_DIV', 'FLOAT_DIV'):
      token = self.current_token
      if token._type == 'MUL':
        self.eat('MUL')
      elif token._type == 'INTEGER_DIV':
        self.eat('INTEGER_DIV')
      elif token._type == 'FLOAT_DIV' :
        self.eat('FLOAT_DIV')
    
      node = BinOp(left=node, op=token, right=self.factor())
    return node

  def expr(self):
    # each BinOp node adopts the current value of the node variable as its
    # left child and the result of a call to a term or factor as its right child, 
    # so it’s effectively pushing down nodes to the left
    node = self.term()
    while self.current_token._type in ('PLUS','MINUS'):
      token = self.current_token
      if token._type == 'PLUS':
        self.eat('PLUS')
      elif token._type == 'MINUS':
        self.eat('MINUS')

      node = BinOp(left=node, op=token, right=self.term())
    
    return node
  
  def parse(self):
    node = self.program()
    if self.current_token._type != 'EOF':
      self.error()
    return node


class NodeVisitor:
  # Visitor Pattern Implementation
  def visit(self, node):
    method_name = 'visit_'+type(node).__name__
    visitor = getattr(self,method_name, self.generic_visit)
    return visitor(node)
  
  def generic_visit(self, node):
    raise Exception(f"No visit_{type(node).__name__}")

class SymbolTableBuilder(NodeVisitor):
  def __init__(self):
    self.symtab = SymbolTable()

  def visit_Program(self, node):
    self.visit(node.block)
  
  def visit_Block(self, node):
    for declaration in node.declarations:
      self.visit(declaration)
    self.visit(node.compound_statement)
  
  def visit_ProcedureDecl(self, node):
    pass

  def visit_BinOp(self, node):
    self.visit(node.left)
    self.visit(node.right)
  
  def visit_Num(self, node):
    pass

  def visit_UnaryOp(self, node):
    self.visit(node.expr)
  
  def visit_Compound(self, node):
    for child in node.children:
      self.visit(child)
  
  def visit_NoOp(self, node):
    pass

  def visit_VarDecl(self, node):
    # First look up the build-in symbol . If it exists , then create a VarSymbol and store in symtable
    type_name = node.type_node.value
    type_symbol = self.symtab.lookup(type_name)
    var_name = node.var_node.value
    var_symbol = VarSymbol(var_name, type_symbol)
    self.symtab.define(var_symbol)
  
  def visit_Assign(self, node):
    var_name = node.left.value
    var_symbol = self.symtab.lookup(var_name)
    if var_symbol is None:
      raise NameError(repr(var_name))
    
    self.visit(node.right)
  
  def visit_Var(self, node):
    var_name = node.value
    var_symbol = self.symtab.lookup(var_name)

    if var_symbol is None:
      raise NameError(repr(var_name))

class Interpreter(NodeVisitor):

  def __init__(self, tree):
    self.tree = tree
    self.GLOBAL_SCOPE = {}
  
  def visit_Program(self, node):
    self.visit(node.block)
  
  def visit_Block(self, node):
    for declaration in node.declarations:
      self.visit(declaration)
    self.visit(node.compound_statement)

  def visit_VarDecl(self, node):
    pass

  def visit_Type(self, node):
    pass
  
  def visit_ProcedureDecl(self, node):
    pass

  def visit_BinOp(self, node):
    if node.op._type == 'PLUS':
      return self.visit(node.left) + self.visit(node.right)
    elif node.op._type == 'MINUS':
      return self.visit(node.left) - self.visit(node.right)
    elif node.op._type == 'MUL':
      return self.visit(node.left) * self.visit(node.right)
    elif node.op._type == 'INTEGER_DIV':
      return self.visit(node.left) / self.visit(node.right)
    elif node.op._type == 'FLOAT_DIV':
      return float(self.visit(node.left)) / float(self.visit(node.right))
  
  def visit_UnaryOp(self, node):
    op = node.op._type
    if op == 'PLUS':
      return +self.visit(node.expr)
    elif op == 'MINUS':
      return -self.visit(node.expr)
  
  def visit_Compound(self, node):
    for child in node.children:
      self.visit(child)
  
  def visit_NoOp(self, node):
    pass

  def visit_Assign(self, node):
    # GLOBAL_SCOPE is a symbol table cum Memory space
    var_name = node.left.value
    self.GLOBAL_SCOPE[var_name] = self.visit(node.right)
  
  def visit_Var(self, node):
    var_name = node.value
    val = self.GLOBAL_SCOPE.get(var_name)
    if val is None:
      raise NameError(repr(var_name))
    else:
      return val

  def visit_Num(self, node):
    return node.value

  def interpret(self):
    tree = self.tree
    if tree is None:
      return ''
    return self.visit(tree)

if __name__ == "__main__":
  # while True:
  #   try :
  #     text = input('calc>')
  #   except EOFError:
  #     break
  #   if not text:
  #     continue
    import sys
    text = open(sys.argv[1], 'r').read()
    lexer = Lexer(text)
    parser=Parser(lexer)
    tree = parser.parse()
    symtab_builder = SymbolTableBuilder()
    symtab_builder.visit(tree)
    print('')
    print('Symbol Table contents:')
    print(symtab_builder.symtab)

    interpreter = Interpreter(tree)
    result = interpreter.interpret()

    print('')
    print('Run-time Global_Memory contents:')
    for k, v in sorted(interpreter.GLOBAL_SCOPE.items()):
        print('{} = {}'.format(k, v))
