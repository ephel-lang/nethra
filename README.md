# Nethra

Nethra is an experiment based on well know theories and constructions like:
- dependent function type,
- dependent pair type,
- recursive type and
- core lambda calculus

## Formal foundations

### Terms

```
n in Ident
i in Int
c in Char
s in String

e ::=
    Type_i                -- Type at level i
    n                     -- Variable
    
    i                     -- Integer literal
    c                     -- Character literal
    s                     -- String literal
    
    Π(n:e).e   Π{n:e}.e   -- Dependant function type
    λ(n).e     λ{n}.e     -- Function
    e e        e {e}      -- Application
    
    Σ(n:e).e              -- Dependant pair type
    e , e                 -- Pair
    fst e                 -- Left projection
    snd e                 -- Right Projection
    
    e + e                 -- Disjunction
    inl e                 -- Left injection
    inr e                 -- Right injection
    case e e e            -- Catamorphism
    
    μ(n).e                -- Recursion
    fold e                -- Fold recursive type
    unfold e              -- Unfold recursive type
    
    e ∈ e                 -- Term inhabits Type
    
    ?n                    -- Hole for inference
```

### Typing rules

#### Hierarchy and hypothesis

```
Γ ⊢
---------------------
Γ ⊢ Type_i : Type_i+1

Γ ⊢
----------------
Γ, x : T ⊢ x : T
```

#### Literals

```
l ∈ int    
-----------
Γ ⊢ l : int

l ∈ char
------------
Γ ⊢ l : char

l ∈ string
--------------
Γ ⊢ l : string
```

#### Dependant function type

```
Γ, x : M ⊢ N : T
----------------
Γ ⊢ Π(x:M).N : T

Γ, x : A ⊢ B : T     
---------------------
Γ ⊢ λ(x).B : Π(x:A).T

Γ, x : A ⊢ B : T
---------------------
Γ ⊢ λ{x}.B : Π{x:A}.T

Γ ⊢ λ{x}.B : Π{x:A}.T   B not a λ
---------------------------------
Γ ⊢ B : Π{x:A}.T

Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M 
----------------------------
Γ ⊢ f e : N[x=e]                        

Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M 
----------------------------
Γ ⊢ f {e} : N[x=e]            

Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
---------------------------------------
Γ ⊢ f e : N
```

#### Dependant pair type

```
Γ,x : A ⊢ B : T
----------------
Γ ⊢ Σ(x:A).B : T

Γ ⊢ A : M   Γ ⊢ B : N[x=A]
--------------------------
Γ ⊢ A,B : Σ(x:M).N

Γ ⊢ p : Σ(x:M).N
----------------
Γ ⊢ fst p : M

Γ ⊢ p : Σ(x:M).N
----------------------
Γ ⊢ snd p : N[x=fst p]
```

#### Disjunction

```
Γ ⊢ A : T   Γ ⊢ B : T
---------------------
Γ ⊢ A + B : T

Γ ⊢ A : M
-----------------
Γ ⊢ inl A : M + N

Γ ⊢ A : N
-----------------
Γ ⊢ inr A : M + N

Γ ⊢ a : A + B   Γ ⊢ l : A -> C   Γ ⊢ r : B -> C
-----------------------------------------------
Γ ⊢ case a l r : C
```

#### Recursion

```
Γ, x : T ⊢ A : T
----------------
Γ ⊢ μ(x).A : T

Γ ⊢ A : N[x=μ(x).N]
-------------------
Γ ⊢ fold A : μ(x).N

Γ ⊢ A : μ(x).N
--------------------------
Γ ⊢ unfold A : N[x=μ(x).N]
```
#### Term inhabitation

```
Γ ⊢ x : T
---------------
Γ ⊢ (x ∈ T) : T
```

## Nethra language in action

### Grammar

```
s0 ::=
    binding*

binding ::= 
    "sig" ID ":" term
    "def" ID "=" term 
```

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
    "rec" "(" id ")" "." sterm
 
    "(" id ")" "." sterm    
    "{" id "}" "." sterm
    
    "type" int?
    
    id
    
    "case" sterm sterm sterm
    "inl" sterm
    "inr" sterm
    
    "fst" sterm
    "snd" sterm
    
    "fold" sterm
    "unfold" sterm
    
    "(" term ")"
    
    ?id
    
    int 
    char
    string
```

### Some examples

#### HKT

```
sig combine : {x:type} -> X -> X -> X
    
sig add : int -> int -> int

sig combineInt : combine {int}   
def combineInt = add
```

#### Function producing HKT

```
sig combine : (x:type) -> type
def combine = (X).(X -> X -> X)
    
sig add : int -> int -> int

sig combineInt : combine int   
def combineInt = add
```

#### Type level programming

In this example with first define a function returning a type depending on the parameter.

Then if the parameter is an `int` it returns the type `char` and if it's a `char` it returns an `int`.

```
sig ic : int | char -> type
def ic = (x).(case x (_).char (_).int)
```

Then such function can be used in type level. For instance the expression `ic (inl 1)` produces the type `char`.

```
sig m1 : ic (inl 1)
def m1 = 'c'
```

#### Dependent pair

In this example, the dependent pair is illustrated thanks to the couple data structure.

```
sig m : (t:type) * t
def m = (char , 'c')
```

##### Trait denotation

###### Naive approach

This can be used to encode modules and records. Then for instance:

```
trait Monoid {
    sig t       : type
    sig empty   : t
    sig compose : t -> t -> t
}
```

can be expressed by the type `(t:type) * (t * (t -> t -> t))`. Of course projections facilities provided by a trait should also be expressed thanks to the couple deconstruction using `fst` and `snd`.

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

###### Extensible approach

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

#### Recursive and disjunctive types

```
sig unit : type
sig Unit : unit
```

```
sig bool : type

sig true : type
sig True : true

sig false : type
sig False : false

def bool = true | false
```

```
sig list : type -> type

sig nil  : {X:type} -> list X
def nil  = {_}.(fold (inl Unit))

sig cons : {X:type} -> X -> list X -> list X
def cons = {_}.(head).(tail).(fold (inr (head,tail)))

def list = (X).rec(l).(unit | (X * l)) 
```

```
sig isEmpty : {X:type} -> list X -> bool
def isEmpty = {_}.(l).case (unfold l) (_).(inl True) (_).(inr False)
```