import sys, sb_langdef

class Scanner():
    def scan(self, src):
        self.src = src
        self.start = 0
        self.current = 0
        self.line = 1
        self.tokens = []
        while not self.eof():
            self.start = self.current
            c = self.peek()
            if c in " \t\r":
                self.advance()
            elif c == "\n":
                self.line += 1
                self.advance()
            elif self.is_op():
                pass
            elif self.is_string():
                pass
            elif self.is_char():
                pass
            elif self.is_number():
                pass
            elif self.is_ident():
                pass
            elif self.is_comment():
                pass
            else:
                self.error(f"unexpected character '{c}'")
        return self.tokens
    def error(self, message):
        print(f"Error: {message} on line {self.line}")
        sys.exit(2)
    def eof(self):
        return not self.current < len(self.src)
    def peek(self):
        return self.src[self.current]
    def advance(self):
        self.current += 1
        return self.src[self.current-1]
    def is_numeral(self, c):
        c = ord(c)
        return c >= ord('0') and c <= ord('9')
    def is_alpha(self, c):
        c = ord(c)
        return (c >= ord('a') and c <= ord('z')) or (c >= ord('A') and c <= ord('Z'))
    def is_alphanumeric(self, c):
        return self.is_numeral(c) or self.is_alpha(c)
    def add_token(self, type = None, lit = None):
        if not lit:
            lit = self.src[self.start:self.current]
        if not type:
            type = lit
        token = {
            "lit": lit,
            "type": type,
            "line": self.line
        }
        self.tokens.append(token)
        return token
    def is_op(self):
        possible = list(filter(lambda op: op[0] == self.peek(), sb_langdef.symbols))
        if len(possible) == 0:
            return False
        self.advance()
        if self.eof():
            self.add_token()
            return True
        poss2 = list(filter(lambda op: len(op) == 2 and op[1] == self.peek(), possible))
        if len(poss2) > 0:
            self.advance()
        self.add_token()
        return True
    def is_string(self):
        if self.peek() != '"':
            return False
        self.advance()
        c = None
        while not self.eof():
            c = self.advance()
            if c == "\n":
                self.error("a newline cannot appear in a string")
            if c == '"':
                break
        if c != '"' and self.eof():
            self.error("reached eof while scanning a string")
        self.add_token("string", self.src[self.start+1:self.current-1])
        return True
    def is_number(self):
        if self.peek() == "-":
            self.advance()
        if not self.is_numeral(self.peek()):
            return False
        while not self.eof() and self.is_numeral(self.peek()):
            self.advance()
        self.add_token("number")
        return True
    def is_ident(self):
        if not (self.is_alpha(self.peek()) or self.peek() == "_"):
            return False
        self.advance()
        while not self.eof() and (self.is_alphanumeric(self.peek()) or self.peek() == "_"):
            self.advance()
        token = self.add_token("identifier")
        if token["lit"] in sb_langdef.keywords or token["lit"] in sb_langdef.stations:
            token["type"] = token["lit"]
        return True
    def is_char(self):
        if self.peek() != "'":
            return False
        self.advance()
        if self.start > len(self.src) - 3:
            self.error(f"unclosed single quote, reached eof")
        c = self.advance()
        end = self.advance()
        if end != "'":
            self.error(f"unclosed character '{c}'")
        self.add_token("char", c)
        return True
    def is_comment(self):
        if self.peek() != "#":
            return False
        while not self.eof() and self.peek() != "\n":
            self.advance()
        return True