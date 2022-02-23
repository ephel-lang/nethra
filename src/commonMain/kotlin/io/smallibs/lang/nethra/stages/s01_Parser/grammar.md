## Nethra grammar

```
s0 ::=
    binding*

binding ::= 
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
```

```
sterm ::=
    "(" id ")" "." sterm    
    "{" id "}" "." sterm
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


