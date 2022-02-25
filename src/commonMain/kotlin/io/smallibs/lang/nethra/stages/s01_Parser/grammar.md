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
    "(" id ":" term ")" ("->" | "*") term    
    "{" id ":" term "}" ("->") term    
    aterm ("->" term)?
```

```
aterm ::=
    sterm ("{" term "}" | sterm)* ("|" aterm)? ((","|"*") aterm)? 
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
    "fst" sterm
    "snd" sterm
    "(" term ")"
    id
    ?id
    Int 
    Char
    String
```

## Nethra Language in action

### HKT

```
sig combine : {x:type} -> X -> X -> X
    
sig add : int -> int -> int

sig combineInt : combine {int}   
def combineInt = add
```

### Function producing HKT

```
sig combine : (x:type) -> type
def combine = (X).(X -> X -> X)
    
sig add : int -> int -> int

sig combineInt : combine int   
def combineInt = add
```

### Type level programming

In this example with first define a function returning a type depending on the parameter.

Then if the parameter is a `int` it returns the type `char` and if it's a char it returns
a `ìnt`. 

```
sig ic : int | char -> type
def ic = (x).(case x (_).char (_).int)
```

Then such function can be used in type level. For instance the expression `ic (inl 1)` produces the type `char`.

```
sig m1 : ic (inl 1)
def m1 = 'c'
```

### Dependent pair

In this example, the dependent pair is illustrated thanks to the couple data structure.

```
sig m : (t:type) * t
def m = (char , 'c')
```

This can be used to encode modules and records. Then for instance:

```
trait A {
    type t
    sig m : t
}
```

can be expressed by the type `(t:type) * t`.