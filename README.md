# Nethra

Nethra is an experiment based on well know theories and constructions like:
- dependent function type,
- dependent pair type,
- recursive type,
- sum type
- core lambda calculus

Some References covering dependent types and type checking in general:

- [The calculus of constructions](https://hal.inria.fr/inria-00076024/document)
- [A simple type-theoretic language: Mini-TT](https://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)
- [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)
- [Cayenne a language with dependent types](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.155&rep=rep1&type=pdf)
- [Implementing Dependent Types in pi-forall](https://arxiv.org/pdf/2207.02129.pdf)
- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
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
    λ(n).e        -- Function
    e e           -- Application
    Π{n:e}.e      -- Dependant function type possibly implicit
    λ{n}.e        -- Function possibly implicit
    e {e}         -- Application possibly implicit
    
    Σ(n:e).e      -- Dependant pair type
    e , e         -- Pair
    fst e         -- Left projection
    snd e         -- Right Projection
    
    e + e         -- Disjunction
    inl e         -- Left injection
    inr e         -- Right injection
    case n e e    -- Catamorphism
    
    μ(n:e).e      -- Recursion
    fold e        -- Fold recursive type
    unfold e      -- Unfold recursive type
    
    (e:e)         -- Type ascription
    
    e = e         -- Equality
    refl          -- Reflexivity   
    subst e by e   -- Substitution 
    
    { n : e, ...} -- Record type
    { n = e, ...} -- Record value
    e.e           -- Field access
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

Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C[a=inl l]   Γ ⊢ r : Π(_:B).T[a=inr r]    a in id
--------------------------------------------------------------------------------
Γ ⊢ case a l r : C

Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C   Γ ⊢ r : Π(_:B).T    a not in id
------------------------------------------------------------------
Γ ⊢ case a l r : C
```

#### Equi-recursive type

```
Γ, x : T ⊢ N : T
----------------
Γ ⊢ μ(x:T).N : T

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
Γ ⊢ (n:M) : M  
```

#### Propositional equality

```
Γ ⊢ n : A    Γ ⊢ m : A
----------------------
Γ ⊢ n = m : Type_0

Γ ⊢
----------------
Γ ⊢ refl : m = m

Γ ⊢ b : x = B    Γ ⊢ a : A[B/x]
-------------------------------    
Γ ⊢ subst a by b : A               

Γ ⊢ b : B = x    Γ ⊢ a : A[B/x]
-------------------------------
Γ ⊢ subst a by b : A
```

#### Record type

```
Γ ⊢ e_i : type_i
----------------------------
Γ ⊢ { n_i : e_i }_i : type_i

Γ ⊢ e_i : T_i
-------------------------------------
Γ ⊢ { n_i = e_i }_i : { n_i : T_i }_i

Γ ⊢ e : { n_i : T_i }_i
-----------------------
Γ ⊢ e . n_i : T_i
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
    term "=" term
    term "->" term
    term "*" term
    term "," term
    term "|" term
    
    term "{" term "}"
    term term
    
    term "@" id

    "(" id ":" term ")" "->" term    
    "{" id ":" term "}" "->" term    
    "(" id ":" term ")" "*" term    
    
    "let" id "=" term "in" term

    "(" id ")" "." term    
    "{" id "}" "." term
    
    "sig" "struct" (id ":" term)*)? "end"
    "val" "struct" (id "=" term)*)? "end"
    
    "case" term term term
    "inl" term
    "inr" term
    
    "fst" term
    "snd" term
    
    "rec" "(" id ")" "." term
    "fold" term
    "unfold" term
    
    "refl"
    "subst" term "by" term

    id
    int 
    char
    string 

    "type" int?
    
    "(" term ")"        
```

### Some examples

#### HKT

```ocaml
sig combine : {x:type} -> X -> X -> X
    
sig add : int -> int -> int

sig combineInt : combine {int}   
val combineInt = add
```

#### Function producing HKT

```ocaml
sig combine : (x:type) -> type
val combine = (X).(X -> X -> X)
    
sig add : int -> int -> int

sig combineInt : combine int   
val combineInt = add
```

#### Type level programming

In this example with first define a function returning a type depending on the parameter.

Then if the parameter is an `int` it returns the type `char` and if it's a `char` it returns an `int`.

```ocaml
sig ic : int | char -> type
val ic = (x).(case x (_).char (_).int)
```

Then such function can be used in type level. For instance the expression `ic (inl 1)` produces the type `char`.

```ocaml
sig m1 : ic (inl 1)
val m1 = 'c'
```

An advanced usage can be proposed thanks to dependent types

```ocaml
sig Unit : type
sig unit : Unit

sig Bool : type
val Bool = Unit | Unit

sig true  : Bool
val true  = inl unit

sig false  : Bool
val false  = inr unit

sig Test : Bool -> type
val Test = (b).case b (_).Unit (_).Bool

sig test : (b:Bool) -> Test b
val test = (b).case b (_).unit (_).true
```

#### Dependent pair

In this example, the dependent pair is illustrated thanks to the couple data structure.

```ocaml
sig m : (t:type) * t
val m = (char , 'c')
```

##### Trait denotation

###### Naive approach

This can be used to encode modules and records. Then for instance a trait like:

```rust
trait Monoid {
    sig empty   : self
    sig compose : self -> self -> self
}
```

can be expressed by the type `(self:type) * (self * (self -> self -> self))`. Of course projections facilities provided by a trait should also be expressed thanks to the couple deconstruction using `fst` and `snd`.

```ocaml
sig Monoid : type
val Monoid = (self:type) * (self * (self -> self -> self))

sig Empty : type
val Empty = (self:type) * self
    
sig Compose : type
val Compose = (self:type) * (self -> self -> self)
```

```ocaml
sig empty : Monoid -> Empty
val empty = (x).(fst x, fst (snd x))

sig compose : Monoid -> Compose
val compose = (x).(fst x, snd (snd x))
```

Then an implementation can be easily done using pairs.

```trust
impl Monoid for int {
    val empty   = 0
    val compose = add   -- int addition
}
```

```ocaml
sig int : type
sig add : int -> int -> int

sig IntMonoid : Monoid
val IntMonoid = (int, 0, add)
```

With this denotation the implementation can't be done using "internal" functions.

###### Extensible approach

For this purpose we can review it adding a polymorphic parameter in order to mimic the row polymorphism.

```ocaml
sig Monoid_T : type -> type
val Monoid_T = (X).((t:type) * t * (t -> t -> t) * X)

sig Empty_T : type
val Empty_T = (t:type) * t
    
sig Compose_T : type
val Compose_T = (t:type) * (t -> t -> t)
```

Then we can propose the functions accessing trait elements.

```ocaml
sig empty : {X:type} -> Monoid_T X -> Empty_T
val empty = (x).(fst x, fst (snd x))

sig compose : {X:type} -> Monoid_T X -> Compose_T
val compose = (x).(fst x, fst (snd (snd x)))
```

With such approach `X` cannot capture the existential type which is not really satisfactory.

#### Recursive sum types

```ocaml
sig list : type -> type

val list = (X).rec(l:type).(unit | (X * l)) 

sig nil  : {X:type} -> list X
val nil  = fold (inl Unit)

sig cons : {X:type} -> X -> list X -> list X
val cons = (head).(tail).(fold (inr (head,tail)))

sig isEmpty : {X:type} -> list X -> bool
val isEmpty = (l).case (unfold l) (_).(inl True) (_).(inr False)
```

#### Encoding sum types

Thanks to `Pi` constructor we are able to encode `data` definition.

```ocaml
sig Atom  : (a:string) -> type
sig data  : {A:type} -> (_:A) -> type
val data  = {A}.(_).A

sig True  : Atom "True"
sig False : Atom "False"

sig bool : type
val bool = data True | data False

sig true : bool
val true = inl True

sig false : bool
val false = inr False
```

#### Dependent type and case

```ocaml
sig Unit : type
sig unit : Unit

sig Bool : type
val Bool = Unit | Unit

sig true  : Bool
val true  = inl unit

sig false  : Bool
val false  = inr unit

sig Test : Bool -> type
val Test = (b).case b (_).Unit (_).Bool

sig test : (b:Bool) -> Test b
val test = (b).case b (_).unit (_).true
```

#### Propositional equality

```ocaml
sig reflexive : {A:type} -> {a:A} -> equals a a
val reflexive = refl

sig symmetric : {A:type} -> {a:A} -> {b:A} -> equals a b -> equals b a
val symmetric = (a_eq_b).subst refl by a_eq_b

sig transitivity : {A:type} -> {a:A} -> {b:A} -> {c:A} -> equals a b -> equals b c -> equals a c
val transitivity = (a_eq_b).(b_eq_c).subst (subst refl by a_eq_b) by b_eq_c
```

#### Leibniz equality

This implementation reproduces the Agda version proposed [here](https://homepages.inf.ed.ac.uk/wadler/papers/leibniz/leibniz.pdf).

```ocaml
sig equal : {A:type} -> (a:A) -> (b:A) -> type
val equal = {A}.(a).(b).((P : A -> type) -> P a -> P b)

sig reflexive : {A:type} -> {a:A} -> equal a a
val reflexive = (P).(Pa).Pa

sig transitive : {A:type} -> {a:A} -> {b:A} -> {c:A} -> equal a b -> equal b c -> equal a c
val transitive = (eq_a_b).(eq_b_c).(P).(Pa).(eq_b_c P (eq_a_b P Pa))

sig symmetric : {A:type} -> {a:A} -> {b:A} -> equal a b -> equal b a
val symmetric = {A}.{a}.(eq_a_b).(P).
    let Qa = reflexive P in
    let Qb = eq_a_b (c).(P c -> P a) Qa in
    Qb
```

#### Record type and value

```ocaml
sig point :type

val point =
    sig struct
        x : int
        y : int
    end

sig zero : point
val zero =
    val struct
        x = 0
        y = 0
    end
```

# Why Nethra?

See [Nethra](https://www.elfdict.com/wt/518511) definition for more information.

# License

MIT License

Copyright (c) 2022 Didier Plaindoux

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
