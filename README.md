# Nethra

Nethra is an experiment based on well-known theories and constructions like:
- dependent function type (Pi type),
- dependent pair type (Sigma type), 
- dependent record (can subsume sigma type),
- dependent recursive type,
- dependent sum type and
- core lambda calculus

## Presentations

- A presentation done for [ScalaIO 2024](https://scala.io/talks/dependent-types-from-theory-to-practice) about this project and dependent types in general is [available](http://d.plaindoux.free.fr/talks/dependent-type/main.html) 
- A presentation is in preparation for [Sunny-Tech 2024](https://sunny-tech.io/sessions/types-dependants-de-la-theorie).

## References

Some References covering dependent types and type checking in general:

- [The calculus of constructions](https://hal.inria.fr/inria-00076024/document)
- [A simple type-theoretic language: Mini-TT](https://www.cse.chalmers.se/~bengt/papers/GKminiTT.pdf)
- [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)
- [Cayenne a language with dependent types](https://dl.acm.org/doi/pdf/10.1145/289423.289451)
- [Implementing Dependent Types in pi-forall](https://arxiv.org/pdf/2207.02129.pdf)
- [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
- [Homotopy Type Theory](https://homotopytypetheory.org/book/)

## Works in progress

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
    Type_i          -- Type at level i
    n               -- Variable
        
    i               -- Integer literal
    c               -- Character literal
    s               -- String literal
    
    Π(n:e).e        -- Dependent function type
    λ(n).e          -- Function
    e e             -- Application
    Π{n:e}.e        -- Dependent function type possibly implicit
    λ{n}.e          -- Function possibly implicit
    e {e}           -- Application possibly implicit
    let n = e in e  -- Let binding 
    
    Σ(n:e).e        -- Dependent pair type
    e , e           -- Pair
    fst e           -- Left projection
    snd e           -- Right Projection
    
    e + e           -- Sum type
    inl e           -- Left injection
    inr e           -- Right injection
    case n e e      -- Catamorphism
    
    μ(n:e).e        -- Recursion
    fold e          -- Fold recursive type
    unfold e        -- Unfold recursive type
    
    (e:e)           -- Type ascription
    
    e = e           -- Equality
    refl            -- Reflexivity   
    subst e by e    -- Substitution 
    
    < n : e, ...>   -- Dependent record type
    { n = e, ...}   -- Record
    e.n             -- Field access
```

### Typing rules

#### Stratified types and cumulatively

By default, the type system is designed on stratified types. 

```
Γ ⊢
-----------------------
Γ ⊢ Type_i : Type_{i+1} 

Γ ⊢ t : Type_i
------------------
Γ ⊢ t : Type_{i+1}
```

#### Type in Type i.e. impredicative

It's also possible to enable the type-in-type capability. In this case, the previous rules can be revisited as follows.

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

#### Dependent function type and application

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
Γ ⊢ f e : N[x:=e]                        

Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M 
----------------------------
Γ ⊢ f {e} : N[x:=e]            

Γ ⊢ e:M   Γ, x:M, x=e ⊢ f : N 
-----------------------------
Γ ⊢ let x = e in f : N[x:=e]            
```

#### Implicit type in Dependent function type and application

```
Γ ⊢ λ{x}.B : Π{x:A}.T   B ≠ λ{y}.C
----------------------------------
Γ ⊢ B : Π{x:A}.T

Γ ⊢ f : Π{x:M}.C   Γ, v:M ⊢ f {v} : N   N ≠ Π{x:T}.T' 
-----------------------------------------------------
Γ ⊢ f : N
```

#### Dependent pair type and deconstructions

```
Γ ⊢ M : Type_i   Γ, x : M ⊢ N : Type_j
--------------------------------------
Γ ⊢ Σ(x:M).N : Type_j

Γ ⊢ A : M   Γ ⊢ B : N[x:=A]
---------------------------
Γ ⊢ A,B : Σ(x:M).N

Γ ⊢ p : Σ(x:M).N
----------------
Γ ⊢ fst p : M

Γ ⊢ p : Σ(x:M).N
-----------------------
Γ ⊢ snd p : N[x:=fst p]
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

Γ ⊢ a : A + B   
Γ ⊢ l : Π(x:A).C[a:=inl x]   Γ ⊢ r : Π(x:B).T[a:=inr x]   a in id, x fresh variable
------------------------------------------------------------------------------------
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

Γ ⊢ A : N[x:=μ(x:T).N]
----------------------
Γ ⊢ fold A : μ(x:T).N

Γ ⊢ A : μ(x:T).N
-----------------------------
Γ ⊢ unfold A : N[x:=μ(x:T).N]
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

Γ ⊢ b : x = B    Γ ⊢ a : A[x:=B]
--------------------------------    
Γ ⊢ subst a by b : A               

Γ ⊢ b : B = x    Γ ⊢ a : A[x:=B]
--------------------------------
Γ ⊢ subst a by b : A
```

#### Dependent record

```
Γ ⊢ 
----------------
Γ ⊢ < > : type_i

Γ ⊢ T : type_i    Γ, n : T ⊢ r : type_i
----------------------------------------
Γ ⊢ < n : T, r > : type_i

Γ ⊢
-------------
Γ ⊢ { } : < >

Γ ⊢ e : T    Γ, n : T ⊢ r : R[n:=e]
-----------------------------------
Γ ⊢ { n = e, r } : < n : T, R >

Γ ⊢ e : T'    Γ, n : T' ⊢ r : < m : T, R >    m ≠ n
---------------------------------------------------
Γ ⊢ { n = e, r } : < m : T, R >

Γ ⊢ e : < n : T, R >
--------------------
Γ ⊢ e.n : T

Γ ⊢ e : < m : T, R >    Γ, m : T ⊢ e.n : R    m ≠ n
---------------------------------------------------
Γ ⊢ e.n : T[m:=e.m]
```

## Toy language in action

This language is based on Nethra.

### Grammar

```
s0 ::=
    binding*

binding ::= 
    "sig" ID ":" term
    "val" ID (":" term)? "=" term 
```

```
term ::=     
    "let" id (":" term)? =" term "in" term
   
    "(" id+ ":" term ")" "->" term    
    "{" id+ ":" term "}" "->" term    
    term "->" term
    term term
    term "{" term "}"

    "(" id+ ":" term ")" "*" term    
    term "*" term
    term "," term
    "fst" term
    "snd" term

    "fun" (id | "{" id+ "}")+ "->" term
    
    "sig" "struct" (id ":" term)* "end"
    "val" "struct" (id "=" term)* "end"
    "#" id term
    
    term "|" term
    "case" term term term
    "inl" term
    "inr" term
    
    "rec" "(" id ":" term ")" "." term
    "fold" term
    "unfold" term
    
    "equals" term term
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
sig combine : (x:type) -> type
val combine = fun X . X -> X -> X
    
sig add : int -> int -> int

sig combineInt : combine int   
val combineInt = add
```

Or with possibly implicit parameters...

```ocaml
sig combine : {x:type} -> X -> X -> X
    
sig add : int -> int -> int

sig combineInt : combine {int}   
val combineInt = add
```

#### Type level programming

In this example we first define a function returning a type depending on the parameter.

Then if the parameter is an `int` it returns the type `char` and if it's a `char` it returns an `int`.

```ocaml
sig ic : int | char -> type
val ic = fun x -> case x (fun _ -> char) (fun _ -> int)
```

Then such function can be used in type level. For instance, the expression `ic (inl 1)` produces the type `char`.

```ocaml
sig m1 : ic (inl 1)
val m1 = 'c'
```

Thanks to dependent types and `case` construction, an advanced usage can be proposed.

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
val Test = fun b -> case b (fun _ -> Unit) (fun _ -> Bool)

sig test : (b:Bool) -> Test b
val test = fun b -> case b (fun _ -> unit) (fun _ -> true)
```

In this example the result of test depends on the parametric boolean. 
Then if the boolean is `true` the type is `unit` and if it's `false`
the type `Bool`.

#### Dependent pair

This example illustrates the dependent pair thanks to the couple data structure.

```ocaml
sig m : (t:type) * t
val m = (char , 'c')
```

##### Trait denotation

###### Naive approach

This can be used to encode modules and records. Then for instance a trait like:

```rust
trait Monoid {
    fn empty() -> Self;
    fn compose(l: Self, r: Self) -> Self;
}
```

can be expressed by the type `(self:type) * (self * (self -> self -> self))`. Of course projections facilities 
provided by a trait should also be expressed thanks to the couple deconstruction using `fst` and `snd`.

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
val empty = fun x -> fst x, fst (snd x)

sig compose : Monoid -> Compose
val compose = fun x -> fst x, snd (snd x)
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

With this denotation, the implementation can't be done using "internal" functions.

###### Extensible approach

For this purpose, we can review it by adding a polymorphic parameter to mimic the row polymorphism.

```ocaml
sig Monoid_T : type -> type
val Monoid_T = fun X -> (t:type) * t * (t -> t -> t) * X

sig Empty_T : type
val Empty_T = (t:type) * t
    
sig Compose_T : type
val Compose_T = (t:type) * (t -> t -> t)
```

Then we can propose the functions accessing trait elements.

```ocaml
sig empty : {X:type} -> Monoid_T X -> Empty_T
val empty = fun x -> fst x, fst (snd x)

sig compose : {X:type} -> Monoid_T X -> Compose_T
val compose = fun x -> fst x, fst (snd (snd x))
```

With such an approach `X` cannot capture the existential type which is not satisfactory.

#### Recursive sum types

```ocaml
sig list : type -> type
val list = fun X -> rec(l:type).(Unit | (X * l)) 

sig nil  : {X:type} -> list X
val nil  = fold (inl unit)

sig cons : {X:type} -> X -> list X -> list X
val cons = fun head tail -> fold (inr (head,tail))

sig isEmpty : {X:type} -> list X -> bool
val isEmpty = fun l -> case (unfold l) (fun _ -> true) (fun _ -> inr false)
```

#### Propositional equality

##### Equality

```ocaml
-{
Propositional equality
}-

sig reflexive :
    {A:type} -> {a:A}
       ----------
    -> equals a a

val reflexive =
    refl

sig symmetric :
    {A:type} -> {a b:A}
    -> equals a b
       ----------
    -> equals b a

val symmetric = fun a=b ->
    subst refl by a=b

sig transitivity :
    {A:type} -> {a b c :A}
    -> equals a b
    -> equals b c
       ----------
    -> equals a c

val transitivity = fun a=b b=c ->
    subst (subst refl by a=b) by b=c
```

##### Congruence and substitution

```ocaml
sig congruent :
    {A B:type} -> (f:A -> B) -> {a b:A}
    -> equals a b
       ------------------
    -> equals (f a) (f b)

val congruent = fun f a=b ->
    subst refl by (a=b)

sig congruent_2 :
    {A B C:type} -> (f:A -> B -> C) -> {a b:A} -> {c d:B}
    -> equals a b
    -> equals c d
       ----------------------
    -> equals (f a c) (f b d)

val congruent_2 = fun f a=b c=d ->
    subst (subst refl by a=b) by c=d

sig congruent_app : {A B:type} -> (f g:A -> B)
    -> equals f g
       ---------------------------
    -> {a:A} -> equals (f a) (g a)

val congruent_app = fun f g f=g ->
    subst refl by (f=g)

sig substitution : {A:type} -> {x y:A} -> (P:A -> type)
    -> equals x y
       ----------
    -> P x -> P y

val substitution = fun P x=y px ->
    subst px by x=y
```

#### Leibniz equality

This implementation reproduces the Agda version proposed [in this paper](https://homepages.inf.ed.ac.uk/wadler/papers/leibniz/leibniz.pdf).

```ocaml
sig equal : {A:type} -> (a:A) -> (b:A) -> type
val equal = fun {A} a b -> (P : A -> type) -> P a -> P b

sig reflexive : {A:type} -> {a:A} -> equal a a
val reflexive = fun P Pa -> Pa

sig transitive : {A:type} -> {a:A} -> {b:A} -> {c:A} -> equal a b -> equal b c -> equal a c
val transitive = fun a=b b=c P Pa -> b=c P (a=b P Pa)

sig symmetric : {A:type} -> {a b:A} -> equal a b -> equal b a
val symmetric = fun {A a b} a=b P ->
    let Q = A -> type in
    let Q = fun c -> P c -> P a in
    let Qa : Q a = reflexive P in
    let Qb : Q b = a=b Q Qa in
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

The object-oriented approach can be "simulated" thanks to structures and recursive type. 

```ocaml
sig int : type
sig add : int -> int -> int

sig point : type
val point =
    rec(self:type).sig struct
        sig x  : int
        sig y  : int
        sig mv : self -> int -> int -> self
    end

sig mkPoint : int -> int -> point
val mkPoint = fun x y ->
    fold val struct
        val x  = x
        val y  = y
        val mv = fun self x y ->
            let nx = add x (#x unfold self) in
            let ny = add y (#y unfold self) in
            (mkPoint nx ny)
    end

sig zero : point
val zero = mkPoint 0 0

sig x : int
val x = #x unfold zero
```

#### Dependent record

```ocaml
sig Monad : (type -> type) -> type
val Monad =
    fun M -> sig struct
        sig map   : {A B:type} -> (A -> B) -> M A -> M B
        sig apply : {A B:type} -> M (A -> B) -> M A -> M B
        sig join  : {A:type} -> M (M A) -> M A
        sig bind  : {A B:type} -> (A -> M B) -> M A -> M B
    end

------------

val Option : type -> type = fun A -> A | Unit
val some : {A:type} -> A -> Option A = fun a -> inl a
val none : {A:type} -> Option A = inr unit

val EitherOption : Monad Option =
    val struct
        val map   = fun {_ B} f ma -> case ma (fun a -> some (f a)) (fun _ -> none {B})
        val apply = fun {_ B} mf ma -> case mf (fun f -> map f ma) (fun _ -> none {B})
        val join  = fun {A} ma -> case ma (fun a -> a) (fun _ -> none {A})
        val bind  = fun f ma -> join (map f ma)
    end

val r : Option Unit = #map EitherOption (fun _ -> unit) (some 1)
```

### First Basic formal proof

```ocaml
sig Unit : type
sig unit : Unit

sig nat : type
val nat = rec(X:type).(Unit | X)

val zero : nat = fold inl unit
val succ : nat -> nat = (n).fold inr n

sig add : nat -> nat -> nat
val add = fun n1 n2 -> case (unfold n1) (fun _ -> n2) (fun n1 -> add n1 n2)

sig Monoid : type
val Monoid =
    sig struct
        sig self : type
        sig neutral : self
        sig combine : self -> self -> self
        -- Monoid Laws
        sig law1 : {a:self} -> equals a (combine neutral a)
    end

sig MonoidNat : Monoid
val MonoidNat =
    val struct
        val self = nat
        val neutral = zero
        val combine = add
        -- Monoid Laws
        val law1 = refl
    end
```

### Encoding GADT

```ocaml
sig Bool : type
sig int  : type

-{
 data Expr A =
 | Boolean of Bool with A = Bool
 | Integer of int  with A = int
}-

sig Expr : type -> type
val Expr = fun A -> (equals A Bool * Bool) | (equals A int * int)

sig boolean : {A:type} -> {_:equals A Bool} -> Bool -> Expr A
val boolean = fun {_ p} b -> inl (p,b)

sig number  : {A:type} -> {_:equals A int}  -> int  -> Expr A
val number  = fun {_ p} b -> inr (p,b)

sig eval : {A:type} -> Expr A -> A
val eval = fun e -> case e (fun e -> subst snd e by fst e) (fun e -> subst snd e by fst e)

-- Usage

val res : int = eval (number 1)
```

Warning: GADT in the presence of a recursive type cannot be expressed (for the moment).

# Why Nethra?

See [Nethra](https://www.elfdict.com/wt/518511) definition for more information.

# License

MIT License

Copyright (c) 2022-2024 Didier Plaindoux

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
