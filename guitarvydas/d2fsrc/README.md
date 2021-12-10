diagram to factbase transpiler

input: draw.io diagram containing tabs containing diagrams

diagram contains only:
- rectangles
- ellipses
- text (value)
- edges


output: facts (1 set of facts per drawio tab)

a `fact` is output in PROLOG format, e.g.
```
fact(relation,subject,object).
```

see run.bash for example usage of d2f
