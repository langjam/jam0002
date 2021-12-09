import re
import json
import sys
pattern = re.compile ("\(([^,]+),([^,]+),([^,]+)\).")
with open('../fb.pl') as f:
    for line in f:
        match = pattern.findall (line)
        print (json.dump (match, sys.stdout), end="\n")
