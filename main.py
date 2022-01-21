from Lexer import my_lexer
from Parser import MyParser
from Lexer import tokens

lexer = my_lexer()
p = MyParser(tokens)
parser = p.parse()

while True:
    code = input('> ')
    if code == "exit":
        break
    if not code:
        continue
    with open(f'TestCodes/{code}.txt', "r") as f:
        parser.parse(f.read())

