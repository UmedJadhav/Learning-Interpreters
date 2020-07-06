
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
  'END': Token('END','END')
}

class Lexer:
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

      if self.current_char.isdigit():
        return Token('INTEGER',self.integer())

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
        return Token('DIV','/')    
    
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

      self.error() # If the token is not any of the recognized one , raise an error

    return Token('EOF',None)

class AST:
  pass

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

class Parser:
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

  def factor(self):
    # factor : (PLUS|MINUS) factor  | INTEGER | LPAREN expr RPAREN 
    token = self.current_token
    if token._type == 'PLUS':
      self.eat('PLUS')
      node = UnaryOp(token, self.factor())
      return node
    elif token._type == 'MINUS':
      self.eat('MINUS')
      node = UnaryOp(token, self.factor())
      return node
    elif token._type == 'INTEGER':
      self.eat('INTEGER')
      return Num(token)
    elif token._type == 'LPAREN':
      self.eat('LPAREN')
      node = self.expr()
      self.eat('RPAREN')
      return node
  
  def term(self):
    # handles term part of grammar term: factor( (MUL | DIV ) factor)
    node = self.factor()
    while self.current_token._type in ('MUL', 'DIV'):
      token = self.current_token
      if token._type == 'MUL':
        self.eat('MUL')
      elif token._type == 'DIV':
        self.eat('DIV')
    
      node = BinOp(left=node, op=token, right=self.factor())
    return node

  def expr(self):
    # Arithmetic expression parser + interpreter
    # Current grammar supported by the parser
    # expr : term((PLUS | MINUS )term )*  * -> any number of arguments 
    # term : factor ((MUL | DIV ) factor)*
    # factor : INTEGER | LPAREN expr RPAREN

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
    return self.expr()


class NodeVisitor:
  # Visitor Pattern Implementation
  def visit(self, node):
    method_name = 'visit_'+type(node).__name__
    visitor = getattr(self,method_name, self.generic_visit)
    return visitor(node)
  
  def generic_visit(self, node):
    raise Exception(f"No visit_{type(node).__name__}")

class Interpreter(NodeVisitor):
  def __init__(self, parser):
    self.parser = parser
  
  def visit_BinOp(self, node):
    if node.op._type == 'PLUS':
      return self.visit(node.left) + self.visit(node.right)
    elif node.op._type == 'MINUS':
      return self.visit(node.left) - self.visit(node.right)
    elif node.op._type == 'MUL':
      return self.visit(node.left) * self.visit(node.right)
    elif node.op._type == 'DIV':
      return self.visit(node.left) / self.visit(node.right)
  
  def visit_UnaryOp(self, node):
    op = node.op._type
    if op == 'PLUS':
      return +self.visit(node.expr)
    elif op == 'MINUS':
      return -self.visit(node.expr)

  def visit_Num(self, node):
    return node.value

  def interpret(self):
    tree = self.parser.parse()
    return self.visit(tree)

if __name__ == "__main__":
  while True:
    try :
      text = input('calc>')
    except EOFError:
      break
    if not text:
      continue
    lexer = Lexer(text)
    parser=Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    print(result)
