```
top ::= 
    "type" ID "=" type
    "sig"  ID ":" type
    "def"  ID "=" expr 
```

```
type ::=
    "(" id ":" type ")" "->" type    
    "{" id ":" type "}" "->" type
    apply (("->" | ",") type)?
```

```
apply ::=
    stype ("{" type "}" | stype)*
```

```
stype ::=
    "*"
    "int"
    "char"
    "(" type ")"
```


