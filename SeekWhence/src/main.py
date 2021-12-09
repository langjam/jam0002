#!/usr/bin/python3

import sys

from core.runtime import exec_file

def usage():
  print('Usage: seekwhence [script path]')
  sys.exit()

if __name__ == "__main__":
  if len(sys.argv) < 2: usage()
  exec_file(sys.argv[1], True)