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
    sterm ("{" term "}" | sterm)* ("*" sterm)? ("->" term)?
```

```
pterm ::=
    "(" id ":" term ")" ("->" | "*") term    
    "{" id ":" term "}" ("->") term    
    "(" id ")" "." term    
    "{" id "}" "." term    
```

```
sterm ::=
    "Type"
    "Int"
    "Char"
    id
    ?id
    "(" term ")"
```


