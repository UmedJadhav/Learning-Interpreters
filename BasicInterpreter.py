class Token:
  def __init__(self, _type, value):
    self._type = _type
    self.value = value

  def __str__(self):
    return f"Token({self._type}, {self.value})"
  
  def __repr__(self):
    return self.__str__()

class Interpreter:
  def __init__(self, text):
    self.text = text
    self.pos = 0 # index into self.text
    self.current_token = None
    self.current_char = self.text[self.pos]

  def error(self):
    raise Exception('Error parsing input')

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
    
      self.error() # If the token is not any of the recognized one , raise an error

    return Token('EOF',None)

  def eat(self, token_type):
    if self.current_token._type == token_type :
      self.current_token = self.get_next_token()
    else:
      self.error()

  def term(self):
    # Returns an INTEGER token value
    token = self.current_token
    self.eat('INTEGER')
    return token.value

  def expr(self):
    # Arithmetic expression parser / interpreter
    self.current_token = self.get_next_token()
    result = self.term()
    while self.current_token._type in ('PLUS','MINUS'):
      token = self.current_token
      if token._type == 'PLUS':
        self.eat('PLUS')
        result = result + self.term()
      elif token._type == 'MINUS':
        self.eat('MINUS')
        result = result -  self.term()
  
    return result


if __name__ == "__main__":
  while True:
    try :
      text = input('calc>')
    except EOFError:
      break
    if not text:
      continue
    interpreter = Interpreter(text)
    result = interpreter.expr()
    print(result)
