## Nethra grammar

```
top ::= 
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
    "case" sterm sterm sterm
    "(" term ")"
    id
    ?id
```


