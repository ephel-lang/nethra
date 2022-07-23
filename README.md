# Nethra

Nethra is an experiment based on well know theories and constructions like:
- dependent function type,
- dependent pair type,
- recursive type and
- core lambda calculus

Some References covering dependent types and more:

- [The calculus of constructions](https://hal.inria.fr/inria-00076024/document)
- [A simple type-theoretic language: Mini-TT](https://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)
- [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)
- [Cayenne a language with dependent types](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.155&rep=rep1&type=pdf)
- [Implementing Dependent Types in pi-forall](https://arxiv.org/pdf/2207.02129.pdf)
- [Homotopy Type Theory](https://homotopytypetheory.org/book/)

## Works in progress

### Extensional equality 

- [Extensional Equality in Intensional Type Theory](http://www.cs.nott.ac.uk/~psztxa/publ/lics99.pdf)

### Module / Trait transformation

### Pattern matching compilation

### Linearity / Affine type

- [Integrating Dependent and Linear Types](https://www.cl.cam.ac.uk/~nk480/dlnl-paper.pdf)

## Formal foundations

### Terms

```
n ∈ Ident
i ∈ Int
c ∈ Char
s ∈ String

e ::=
    Type_i        -- Type at level i
    n             -- Variable
        
    i             -- Integer literal
    c             -- Character literal
    s             -- String literal
    
    Π(n:e).e      -- Dependant function type
    Π{n:e}.e      -- Dependant function type possibly implicit
    λ(n).e        -- Function
    λ{n}.e        -- Function possibly implicit
    e e           -- Application
    e {e}         -- Application possibly implicit
    
    Σ(n:e).e      -- Dependant pair type
    e , e         -- Pair
    fst e         -- Left projection
    snd e         -- Right Projection
    
    e + e         -- Disjunction
    inl e         -- Left injection
    inr e         -- Right injection
    case e e e    -- Catamorphism
    
    μ(n:e).e      -- Recursion
    fold e        -- Fold recursive type
    unfold e      -- Unfold recursive type
    
    e as e        -- Type annotation
```

### Typing rules

#### Stratified types and cumulativity

By default, the type system is design on top of stratified types. 

```
Γ ⊢
-----------------------
Γ ⊢ Type_i : Type_{i+1}

Γ ⊢ t : Type_i
------------------
Γ ⊢ t : Type_{i+1}
```

#### Type in Type i.e. impredicative

It's also possible to enable the type in type capability. In this case, the previous rules can be revisited as follows.

```
Γ ⊢
-------------------
Γ ⊢ Type_i : Type_j
```

#### Hypothesis

```
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

#### Dependant function type and application

```
Γ ⊢ M : Type_i   Γ, x : M ⊢ N : Type_j
--------------------------------------
Γ ⊢ Π(x:M).N : Type_j

Γ, x : A ⊢ B : T     
---------------------
Γ ⊢ λ(x).B : Π(x:A).T

Γ, x : A ⊢ B : T
---------------------
Γ ⊢ λ{x}.B : Π{x:A}.T

Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M 
----------------------------
Γ ⊢ f e : N[x=e]                        

Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M 
----------------------------
Γ ⊢ f {e} : N[x=e]            

```

#### Implicit type in Dependant function type and application

```
Γ ⊢ λ{x}.B : Π{x:A}.T   B ≠ λ{y}.C
----------------------------------
Γ ⊢ B : Π{x:A}.T

Γ ⊢ f : Π{x:M}.C   Γ, v:M ⊢ f {v} : N   N ≠ Π{x:T}.T' 
-----------------------------------------------------
Γ ⊢ f : N
```

#### Dependant pair type and deconstructions

```
Γ ⊢ M : Type_i   Γ, x : M ⊢ N : Type_j
--------------------------------------
Γ ⊢ Σ(x:M).N : Type_j

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

#### Disjunction and injections

```
Γ ⊢ A : Type_i   Γ ⊢ B : Type_i
-------------------------------
Γ ⊢ A + B : Type_i

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

#### Equi-recursive type

```
Γ, x : T ⊢ A : T
----------------
Γ ⊢ μ(x:T).A : T

Γ ⊢ A : N[x=μ(x:T).N]
---------------------
Γ ⊢ fold A : μ(x:T).N

Γ ⊢ A : μ(x:T).N
----------------------------
Γ ⊢ unfold A : N[x=μ(x:T).N]
```

#### Type annotation

```
Γ ⊢ n : M    Γ ⊢ M : Type_0    
---------------------------
Γ ⊢ n as M : M  
```

## Nethra Toy language in action

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
    aterm (("->"|"*"|",") term)?
```

```
aterm ::=
    sterm ("{" term "}" | sterm)* 
```

```
sterm ::=
    "let" id "=" term "in" sterm

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

This can be used to encode modules and records. Then for instance a trait like:

```
trait Monoid {
    sig empty   : self
    sig compose : self -> self -> self
}
```

can be expressed by the type `(self:type) * (self * (self -> self -> self))`. Of course projections facilities provided by a trait should also be expressed thanks to the couple deconstruction using `fst` and `snd`.

```
sig Monoid : type
def Monoid = (self:type) * (self * (self -> self -> self))

sig Empty : type
def Empty = (self:type) * self
    
sig Compose : type
def Compose = (self:type) * (self -> self -> self)
```

```
sig empty : Monoid -> Empty
def empty = (x).(fst x, fst (snd x))

sig compose : Monoid -> Compose
def compose = (x).(fst x, snd (snd x))
```

Then an implementation can be easily done using pairs.

```
impl Monoid for int {
    def empty   = 0
    def compose = add   -- int addition
}
```

```
sig int : type
sig add : int -> int -> int

sig IntMonoid : Monoid
def IntMonoid = (int, 0, add)
```

With this denotation the implementation can't be done using "internal" functions.

###### Extensible approach

For this purpose we can review it adding a polymorphic parameter in order to mimic the row polymorphism.

```
sig Monoid_T : type -> type
def Monoid_T = (X).((t:type) * t * (t -> t -> t) * X)

sig Empty_T : type
def Empty_T = (t:type) * t
    
sig Compose_T : type
def Compose_T = (t:type) * (t -> t -> t)
```

Then we can propose the functions accessing trait elements.

```
sig empty : {X:type} -> Monoid_T X -> Empty_T
def empty = (x).(fst x, fst (snd x))

sig compose : {X:type} -> Monoid_T X -> Compose_T
def compose = (x).(fst x, fst (snd (snd x)))
```

With such approach `X` cannot capture the existential type which is not really satisfactory.

#### Recursive and sum types

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

def list = (X).rec(l:type).(unit | (X * l)) 

sig nil  : {X:type} -> list X
def nil  = fold (inl Unit)

sig cons : {X:type} -> X -> list X -> list X
def cons = (head).(tail).(fold (inr (head,tail)))
```

```
sig isEmpty : {X:type} -> list X -> bool
def isEmpty = (l).case (unfold l) (_).(inl True) (_).(inr False)
```

#### Liebnitz equality

```
sig eq : {A:type} -> (a:A) -> (b:A) -> type
def eq = {A}.(a).(b).((P : A -> type) -> P a -> P b)

sig reflexive : {A:type} -> {a:A} -> eq a a
def reflexive = (P).(Pa).Pa

sig transitive : {A:type} -> {a:A} -> {b:A} -> {c:A} -> eq a b -> eq b c -> eq a c
def transitive = (eq_a_b).(eq_b_c).(P).(Pa).(eq_b_c P (eq_a_b P Pa))

sig symmetric : {A:type} -> {a:A} -> {b:A} -> eq a b -> eq b a
def symmetric = {A}.{a}.{b}.(eq_a_b).(P).
    let Qa = reflexive P in
    let Qb = eq_a_b (c).(P c -> P a) Qa in
    Qb
```
