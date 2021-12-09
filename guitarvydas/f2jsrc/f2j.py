import re
import json
import sys
pattern = re.compile ("\(([^,]+),([^,]+),([^,]+)\).")
stringInString = re.compile ("[\"]");
fb=[]
for line in sys.stdin:
    match = pattern.search (line)
    rel = match.group (1).strip ()
    subj = match.group (2).strip ()
    obj = match.group (3)
    if ("\"" in obj):
        obj = eval (obj)
    else:
        obj = obj.strip ()
        if (obj.isdecimal ()):
            obj = int (obj)
    fact = { 'relation' : rel, 'subject' : subj, 'object' : obj }
    fb.append (fact)
json.dump (fb, sys.stdout)
