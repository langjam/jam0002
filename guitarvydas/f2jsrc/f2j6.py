import re
import json
import sys
pattern = re.compile ("\(([^,]+),([^,]+),([^,]+)\).")
stringInString = re.compile ("[\"]");
fb=[]
with open('../fb.pl') as f:
    for line in f:
        match = pattern.search (line)
        rel = match.group (1)
        subj = match.group (2)
        obj = match.group (3)
        if ("\"" in obj):
            obj = eval (obj)
        else:
            if (obj.isdecimal ()):
                obj = eval (obj)
        fact = { 'relation' : rel, 'subject' : subj, 'object' : obj }
        fb.append (fact)
json.dump (fb, sys.stdout)
