## Nethra grammar

```
s0 ::=
    entity*

entity ::= 
    "sig" ID ":" term
    "def" ID "=" term 
```

## Term grammar

```
term ::=
    pterm
    sterm ("{" term "}" | sterm)* ("*" sterm)? ("|" sterm)? ("->" term)?
```

```
pterm ::=
    "(" id ":" term ")" ("->" | "*") term    
    "{" id ":" term "}" ("->") term    
    "(" id ")" "." sterm    
    "{" id "}" "." sterm
```

```
sterm ::=
    "Type"
    "Int"
    "Char"
    "String"
    "case" sterm sterm sterm
    "(" term ")"
    id
    ?id
    Int 
    Char
    String
```


