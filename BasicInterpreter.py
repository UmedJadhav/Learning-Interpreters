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

  def error(self):
    raise Exception('Error parsing input')


  def get_next_token(self):
    text = self.text
    if self.pos > len(text) - 1 :
      return Token('EOF', None)
    
    curr_char = text[self.pos]

    if curr_char.isdigit():
      token = Token('INTEGER',int(curr_char))
      self.pos += 1
      return token
    
    if curr_char ==  '+':
      token = Token('PLUS',curr_char)
      self.pos += 1
      return token
    
    self.error() # If the token is not any of the recognized one , raise an error

  def eat(self, token_type):
    if self.current_token._type == token_type :
      self.current_token = self.get_next_token()
    else:
      self.error()
    
  def expr(self):
    self.current_token = self.get_next_token()
    left = self.current_token
    self.eat('INTEGER')

    op = self.current_token
    self.eat('PLUS')

    right = self.current_token
    self.eat('INTEGER')

    result = left.value + right.value
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
