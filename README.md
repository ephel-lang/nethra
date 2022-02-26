# Nethra

Nethra is an experiment based on well know theory and constructions:
- dependent function type,
- dependent pair type,
- recursive type and
- basic lambda calculus

## Terms

```
n in Ident
i in Int
c in Char

e ::=
    Type_i                -- Type at level i
    n                     -- Variable
    
    data(n:e)             -- Constructor ???
    i                     -- Integer literal
    c                     -- Character literal
    
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

## Typing rules

### Hierarchy and hypothesis

```
Γ ⊢
---------------------
Γ ⊢ Type_i : Type_i+1

Γ ⊢
----------------
Γ, x : T ⊢ x : T
```

### Literals and constructors

```
l ∈ int    
-----------
Γ ⊢ l : int

l ∈ char
------------
Γ ⊢ l : char

Γ ⊢
-----------------
Γ ⊢ data(n:T) : T
```

### Dependant function type

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

### Dependant pair type

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

### Disjunction

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

### Recursion

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
### Term inhabitation

```
Γ ⊢ x : T
---------------
Γ ⊢ (x ∈ T) : T
```
