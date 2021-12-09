convert a factbase into JSON

usage: f2j <fb.pl >fb.json

(here, "factbase" means a PROLOG-style fact)

a fact is a triple
1. relation
2. subject
3. object

relation is always an identifier
subject is always an identifier
object is a number or a string or an identifier - in JSON, this becomes: number, string, string
