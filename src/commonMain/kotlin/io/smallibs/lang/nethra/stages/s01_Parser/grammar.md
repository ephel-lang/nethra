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
    aterm ("->" term)?
```

```
aterm ::=
    sterm ("{" term "}" | sterm)* ("*" aterm)? ("|" aterm)?
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
    "type" int?
    "int"
    "char"
    "string"
    "case" sterm sterm sterm
    "inl" sterm
    "inr" sterm
    "(" term ")"
    id
    ?id
    Int 
    Char
    String
```

## Examples

```
sig combine : (x:type) -> type
def combine = (X).(X -> X -> X)
    
sig add : int -> int -> int

sig combineInt : combine int   
def combineInt = add
```

