module Impl (Checker : Specs.Checker) = struct
  include Goal
  open Stdlib.Fun
  open Preface.Option.Monad
  open Preface.Option.Foldable
  open Nethra_ast.Term.Builders
  open Nethra_ast.Term.Catamorphism
  open Nethra_ast.Proof.Builders
  open Nethra_ast.Bindings.Access
  open Reduction
  open Substitution
  open Checker

  let proof_from_option ?(reason = None) o proof =
    fold_right const o (None, [ proof; failure reason ])

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let infer_kind _bindings (level, c) =
    let term = kind ~c @@ (level + 1) in
    (Some term, [])

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let infer_int _bindings (_, c) =
    let term = id ~c "int" in
    (Some term, [])

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let infer_char _bindings (_, c) =
    let term = id ~c "char" in
    (Some term, [])

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let infer_string _bindings (_, c) =
    let term = id ~c "string" in
    (Some term, [])

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let infer_id bindings (name, _, _) =
    match get_signature bindings name with
    | Some term -> (Some term, [])
    | None -> (None, [ failure @@ Some ("Unbound variable " ^ name) ])

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Π(x:M).N : T
  *)
  let rec infer_pi bindings (name, bound, body, _implicit, _c) =
    let bound', proof = bindings |- bound <:?> () in
    let body', proof' = add_signature bindings (name, bound) |- body <:?> () in
    (bound' >>= const body', [ proof; proof' ])

  (*
    Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    ---------------------     ---------------------
    Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
  *)
  and infer_lambda _bindings (_name, _body, _implicit, _c) =
    (None, [ failure @@ Some "TODO" ])

  (*
    Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M
    ----------------------------      ----------------------------
    Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]
  *)
  and infer_apply bindings (abstraction, argument, implicit, _c) =
    let pi, proof = bindings |- abstraction <:?> () in
    proof_from_option
      ( pi
      >>= fold_opt ~pi:(fun t -> Some t)
      <&> fun (n, bound, body, implicit', _c') ->
      if implicit = implicit'
      then
        (Some (substitute n argument body), [ bindings |- argument <?:> bound ])
      else (None, [ proof; failure None ]) )
      proof

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Σ(x:M).N : T
  *)
  and infer_sigma bindings (name, bound, body, _c) =
    let bound', proof = bindings |- bound <:?> () in
    let body', proof' = add_signature bindings (name, bound) |- body <:?> () in
    (bound' >>= const body', [ proof; proof' ])

  (*
    Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    --------------------------
    Γ ⊢ A,B : Σ(x:M).N
  *)
  and infer_pair bindings (lhd, rhd, _c) =
    let left, proof = bindings |- lhd <:?> () in
    let right, proof' = bindings |- rhd <:?> () in
    ( (left >>= fun left -> right <&> fun right -> sigma "_" left right)
    , [ proof; proof' ] )

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------
    Γ ⊢ fst p : M
  *)
  and infer_fst bindings (term, _c) =
    let sigma, proof = bindings |- term <:?> () in
    proof_from_option ~reason:(Some "Waiting for a sigma")
      ( sigma
      >>= fold_opt ~sigma:(fun t -> Some t)
      <&> fun (_, bound, _, _) -> (Some bound, [ proof ]) )
      proof

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------------
    Γ ⊢ snd p : N[x=fst p]
  *)
  and infer_snd bindings (term, _c) =
    let sigma, proof = bindings |- term <:?> () in
    proof_from_option ~reason:(Some "Waiting for a sigma")
      ( sigma
      >>= fold_opt ~sigma:(fun t -> Some t)
      <&> fun (n, _, body, _) -> (Some (substitute n (fst term) body), [ proof ])
      )
      proof

  (*
    Γ ⊢ A : T   Γ ⊢ B : T
    ---------------------
    Γ ⊢ A + B : T
  *)
  and infer_sum bindings (lhd, rhd, _c) =
    let term, proof = bindings |- lhd <:?> () in
    proof_from_option
      (term <&> fun term -> (Some term, [ bindings |- rhd <?:> term ]))
      proof

  (*
    Γ ⊢ A : M
    -----------------
    Γ ⊢ inl A : M + N
  *)
  and infer_inl bindings (term, c) =
    let term, proof = bindings |- term <:?> () in
    proof_from_option
      ( term
      <&> fun term ->
      let var, _bindings = fresh_variable bindings "_" in
      (Some (sum term (hole ~c var)), [ proof ]) )
      proof

  (*
    Γ ⊢ A : N
    -----------------
    Γ ⊢ inr A : M + N
  *)
  and infer_inr bindings (term, c) =
    let term, proof = bindings |- term <:?> () in
    proof_from_option
      ( term
      <&> fun term ->
      let var, _bindings = fresh_variable bindings "_" in
      (Some (sum (hole ~c var) term), [ proof ]) )
      proof

  (*
    Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C   Γ ⊢ r : Π(_:B).T
    ---------------------------------------------------
    Γ ⊢ case a l r : C
  *)
  and infer_case _bindings (_term, _left, _right, _c) =
    (None, [ failure @@ Some "TODO" ])

  (*
    Γ,x : T ⊢ A : T
    ---------------
    Γ ⊢ μ(x).A : T
  *)
  and infer_mu bindings (name, body, c) =
    let var, bindings = fresh_variable bindings name in
    let bindings = add_signature bindings (name, hole ~c var) in
    let term, proof = bindings |- body <:?> () in
    (term, [ proof ])

  (*
    Γ ⊢ A : N[x=μ(x).N]
    -------------------
    Γ ⊢ fold A : μ(x).N
  *)
  and infer_fold _bindings (_term, _c) =
    (None, [ failure @@ Some "Cannot infer fold" ])

  (*
    Γ ⊢ A : μ(x).N
    --------------------------
    Γ ⊢ unfold A : N[x=μ(x).N]
  *)
  and infer_unfold bindings (term, _c) =
    let term, proof = bindings |- term <:?> () in
    proof_from_option
      ( term
      >>= fold_opt ~mu:(fun t -> Some t)
      <&> fun (n, body, c) ->
      (Some (substitute n (mu ~c n body) body), [ proof ]) )
      proof

  (*
    Γ ⊢                   Γ, x : T ⊢ U : T
    -----------------     -------------------
    Γ, x : T ⊢ ?x : T     Γ, x : T ⊢ ?x=U : T
  *)
  and infer_hole _bindings (_name, _value, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_type bindings term =
    let term', proofs =
      fold ~kind:(infer_kind bindings) ~int:(infer_int bindings)
        ~char:(infer_char bindings) ~string:(infer_string bindings)
        ~id:(infer_id bindings) ~pi:(infer_pi bindings)
        ~lambda:(infer_lambda bindings) ~apply:(infer_apply bindings)
        ~sigma:(infer_sigma bindings) ~pair:(infer_pair bindings)
        ~fst:(infer_fst bindings) ~snd:(infer_snd bindings)
        ~sum:(infer_sum bindings) ~inl:(infer_inl bindings)
        ~inr:(infer_inr bindings) ~case:(infer_case bindings)
        ~mu:(infer_mu bindings) ~fold:(infer_fold bindings)
        ~unfold:(infer_unfold bindings) ~hole:(infer_hole bindings) term
    in
    (term' <&> reduce bindings, infer term term' proofs)

  and ( <:?> ) (bindings, term) () = infer_type bindings term
end
