class Token:
  def __init__(self, _type, value):
    self._type = _type
    self.value = value

  def __str__(self):
    return f"Token({self._type}, {self.value})"
  
  def __repr__(self):
    return self.__str__()

class Lexer:
  def __init__(self, text):
    self.text = text
    self.pos = 0 # index into self.text
    self.current_char = self.text[self.pos]

  def error(self):
    raise Exception('Invalid Syntax')

  def advance(self):
    self.pos += 1
    if self.pos > len(self.text) - 1:
      self.current_char = None
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
  
class Num(AST):
  def __init__(self, token):
    self.token = token
    self.value= token.value
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
    # factor : INTEGER '
    token = self.current_token
    if token._type == 'INTEGER':
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
  # Interpreter
  def visit(self, node):
    method_name = 'visit_'+type(node).__name__
    visitor = getattr(self,method_name, self.generic_visit)
    return visitor(node)
  
  def generic_visit(self, node):
    raise Exception(f"No visit_{type(node).__name__}")

class Interpreter(NodeVisitor):
  def __init__(self, parser):
    self.parser = parser
  
  def visit_Binop(self, node):
    if node.op.type == 'PLUS':
      return self.visit(node.left) + self.visit(node.right)
    elif node.op.type == 'MINUS':
      return self.visit(node.left) - self.visit(node.right)
    elif node.op.type == 'MUL':
      return self.visit(node.left) * self.visit(node.right)
    elif node.op.type == 'DIV':
      return self.visit(node.left) / self.visit(node.right)
  
  def visit_Num(self, node):
    return node.value

  def interpret(self):
    tree = self.parse.parser
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
    interpreter = Interpreter(lexer)
    result = interpreter.expr()
    print(result)
