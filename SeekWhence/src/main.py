#!/usr/bin/python3

import sys

from core.sequence import *
from core.parser import *
from core.runtime import *

def usage():
  print(f'Usage: {sys.argv[0]} [script_path]')
  sys.exit()

if __name__ == "__main__":
  if len(sys.argv) < 2: usage()
  exec_file(sys.argv[1], True)