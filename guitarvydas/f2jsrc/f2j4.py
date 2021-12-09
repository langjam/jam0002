import re
import json
import sys
pattern = re.compile ("\(([^,]+),([^,]+),([^,]+)\).")
with open('../fb.pl') as f:
    for line in f:
        match = pattern.search (line)
        fact = { 'relation' : match.group (1), 'subject' : match.group (2), 'object' : match.group (3) }
        print (fact)
        json.dump (fact, sys.stdout)
