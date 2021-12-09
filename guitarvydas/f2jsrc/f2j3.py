import re
import json
import sys
pattern = re.compile ("\(([^,]+),([^,]+),([^,]+)\).")
with open('../fb.pl') as f:
    for line in f:
        match = pattern.search (line)
        fact = { 'relation' : match.group (1), 'subject' : match.group (1), 'object' : match.group (2) }
        print (fact)
