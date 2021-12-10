from sb_scanner import Scanner
from sb_parser import Parser
from sb_interpreter import Interpreter
import sys

def main():
    s = Scanner()
    p = Parser()
    i = Interpreter()
    args = sys.argv[1:]
    if len(args) == 0:
        print("Thanks for trying the SteamBoat language! To compile and run a sb program supply the path as an argument.")
        return
    with open(args[0]) as f:
        tokens = s.scan(f.read())
        ast = p.parse(tokens)
        i.run(ast)

if __name__ == "__main__":
    main()