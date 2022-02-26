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
    rec(id).sterm
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
    "fold" sterm
    "unfold" sterm
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

#### Trait denotation

##### Naive approach

This can be used to encode modules and records. Then for instance:

```
trait Monoid {
    sig t       : type
    sig empty   : t
    sig compose : t -> t -> t
}
```

can be expressed by the type `(t:type) * (empty:t).* (compose : t -> t -> t))`. Of course projections facilities provided by a trait should also be expressed thanks to the couple deconstruction using `fst` and `snd`.

```
sig Monoid_T : type
def Monoid_T = (t:type) * (t * (t -> t -> t))

sig Empty_T : type
def Empty_T = (t:type) * t
    
sig Compose_T : type
def Compose_T = (t:type) * (t -> t -> t)
```

```
sig empty : Monoid_T -> Empty_T
def empty = (x).(fst x, fst (snd x))

sig compose : Monoid_T -> Compose_T
def compose = (x).(fst x, snd (snd x))
```

With this denotation the implementation can't be done using internal functions.

##### Extensible approach

For this purpose we can review it adding a polymorphic parameter in order to mimic the row polymorphism.

```
sig Monoid_T : type -> type
def Monoid_T = (X).((t:type) * (t * (t -> t -> t) * X))

sig Empty_T : type
def Empty_T = (t:type) * t
    
sig Compose_T : type
def Compose_T = (t:type) * (t -> t -> t)
```

Then we can propose the functions accessing trait elements. 

```
sig empty : {X:type} -> Monoid_T X -> Empty_T
def empty = {_}.(x).(fst x, fst (snd x))

sig compose : {X:type} -> Monoid_T X -> Compose_T
def compose = {_}.(x).(fst x, fst snd (snd x))
```

With such approach `X` cannot capture the existential type which is not really satisfactory. 