from os import error
import sys
import sb_langdef

class Parser():
    def parse(self, tokens):
        self.tokens = tokens
        self.current = 0
        self.current_island = None
        self.current_port = None
        self.current_lock = None
        self.islands = {}
        # body, canal = self.pattern()
        # print(body, canal)
        self.island()
        return self.islands
    def eof(self):
        return not self.current < len(self.tokens)
    def peek(self):
        if self.eof():
            self.error("unexected eof reached")
        return self.tokens[self.current]
    def peektype(self):
        return self.peek()["type"]
    def advance(self):
        self.current += 1
        return self.tokens[self.current - 1]
    def match(self, types):
        if self.peek()["type"] in types:
            self.advance()
            return True
        return False
    def consume(self, type, message):
        if self.eof() or self.peek()["type"] != type:
            self.error(message)
        self.advance()
    def error(self, message):
        if self.eof():
            print(f"Syntax error: {message} at eof.")
        else:
            lit = self.peek()["lit"]
            line = self.peek()["line"]
            print(f"Syntax error: {message} at token '{lit}' on line {line}.")
        sys.exit(2)
    def satement(self):
        if self.eof():
            return
        t = self.peektype()
        if t == "island":
            self.island()
        elif t == "port":
            self.port()
        elif t == "lock":
            self.lock()
    def island(self):
        self.consume("island", "steamboat file must begin with 'island' keyword")
        #self.current_island = self.advance()[]
        if self.peektype() != "identifier":
            self.error("identifier expected after 'island' keyword")
        self.current_island = self.advance()["lit"]
        self.islands[self.current_island] = {
            "name": self.current_island,
            "buffer": [0 for _ in range(1024)],
            "ptr": 0,
            "ports": {},
            "locks": []
        }
        self.port()
    def port(self):
        self.consume("port", "expected keyword 'port'")
        if self.peektype() != "identifier":
            self.error("identifier expected after 'port' keyword")
        self.current_port = self.advance()["lit"]
        island = self.islands[self.current_island]
        island["ports"][self.current_port] = len(island["locks"])
        self.lock()
    def lock(self):
        self.consume("lock", "expected keyword 'lock'")
        island = self.islands[self.current_island]
        island["locks"].append(self.pattern())
        self.satement()
    def gate(self):
        out = None
        t = self.peektype()
        if t == "_":
            out =["wildcard"]
        elif t == "number":
            out =["lit", int(self.peek()["lit"])]
        elif t == "char":
            out =["lit", ord(self.peek()["lit"])]
        elif t == "identifier":
            out =["identifier", self.peek()["lit"]]
        else:
            return None
        self.advance()
        return out
    def pattern(self):
        body = []
        while not self.eof():
            gate = self.gate()
            if gate:
                body.append(gate)
            elif self.peektype() == "string":
                for c in self.peek()["lit"]:
                    body.append(["lit", ord(c)])
                self.advance()
            elif self.match(["."]):
                body.append(["empty"])
            elif self.peektype() == "[":
                self.advance()
                if self.peektype() == "]":
                    body.append(["matchall", ["wildcard"]])
                    self.advance()
                else:
                    temp = []
                    if self.peektype() == "_":
                        temp.append("matchall")
                    elif self.peektype() == "number":
                        temp.append(int(self.peek()["lit"]))
                    else:
                        self.error("array match must begin with '_' or number")
                    self.advance()
                    self.consume(",", "expected comma in pattern")
                    gate = self.gate()
                    if not gate:
                        self.error("gate expected")
                    temp.append(gate)
                    body.append(temp)
                    self.advance()
            else:
                self.consume("canal", "expected 'canal' keyword after pattern")
                break
        canal = self.canal()
        return body, canal

    def canal(self):
        body = []
        while not self.eof():
            if self.match(["lit"]):
                if self.match(["["]):
                    while not self.eof():
                        if self.match(["]"]):
                            break
                        elif self.peektype() == "char":
                            body.append(["lit", ord(self.advance()["lit"])])
                        elif self.peektype() == "number":
                            body.append(["lit", int(self.advance()["lit"])])
                        else:
                            print(self.peek())
                            self.error(f"array elements must be characters or numbers not {self.peektype()}")
                        if not self.peektype() == "]":
                            self.consume(",", "elements in arrays must be separated by commas")
                elif self.peektype() == "string":
                    for c in reversed(self.peek()["lit"]):
                        body.append(["lit", c])
                elif self.peektype() == "char":
                    body.append(["lit", self.advance()["lit"]])
                elif self.peektype() == "number":
                    body.append(["lit", int(self.advance()["lit"])])
            elif self.match(["goto"]):
                port = self.peek()
                ptype = port["type"]
                port = port["lit"]
                self.consume("identifier", f"expected identifier as destination island, got {ptype}")
                #self.consume(",", "a comma is expected in a destination, between the island and port")
                island = None
                if not self.eof() and self.peektype() == ",":
                    self.advance()
                    island = self.peek()
                    itype = island["type"]
                    island = island["lit"]
                    self.consume("identifier", f"expected identifier as destination port, got {itype}")
                body.append(["goto", island, port])
            elif self.match(["chars"]):
                s = self.peek()
                self.consume("string", "expected string after 'chars' keyword")
                for c in s["lit"]:
                    body.append(["lit", ord(c)])
            elif self.peektype() in sb_langdef.stations:
                body.append([self.advance()["type"]])
            else:
                break
                #self.error("unexpected token")
        if len(body) == 0:
            self.error("expected canal")
        return body