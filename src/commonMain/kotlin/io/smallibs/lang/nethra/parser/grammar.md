## Nethra grammar

```
top ::= 
    "type" ID "=" type
    "sig"  ID ":" type
    "def"  ID "=" expr 
```

## Type grammar

```
type ::=
    ptype
    stype ("{" type "}" | stype)* ("*" stype)? ("->" type)?
```

```
ptype ::=
    "(" id ":" type ")" ("->" | "*") type    
    "{" id ":" type "}" "->" type    
```

```
stype ::=
    "Type"
    "Int"
    "Char"
    id
    ?id
    "(" type ")"
```


