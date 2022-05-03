module Impl (Checker : Specs.Checker) = struct
  include Judgment
  open Stdlib.Fun
  open Preface.Option.Monad
  open Preface.Option.Foldable
  open Nethra_lang_ast.Term.Construct
  open Nethra_lang_ast.Term.Destruct
  open Nethra_lang_ast.Proof.Construct
  open Nethra_lang_ast.Context.Hypothesis.Access
  open Nethra_lang_basic.Substitution
  open Reduction
  open Checker

  let proof_from_option ?(reason = None) o proofs =
    fold_right const o (None, proofs @ [ failure reason ])

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let infer_kind _hypothesis (level, c) =
    let term = kind ~c @@ (level + 1) in
    (return term, [])

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let infer_int _hypothesis (_, c) =
    let term = id ~c "int" in
    (return term, [])

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let infer_char _hypothesis (_, c) =
    let term = id ~c "char" in
    (return term, [])

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let infer_string _hypothesis (_, c) =
    let term = id ~c "string" in
    (return term, [])

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let infer_id hypothesis (name, _, _) =
    proof_from_option
      ~reason:(return ("Unbound variable " ^ name))
      (get_signature hypothesis name <&> fun term -> (return term, []))
      []

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Π(x:M).N : T
  *)
  let rec infer_pi hypothesis (name, bound, body, _implicit, _c) =
    let bound', proof = hypothesis |- bound => () in
    let body', proof' = add_signature hypothesis (name, bound) |- body => () in
    (bound' >>= const body', [ proof; proof' ])

  (*
    Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    ---------------------     ---------------------
    Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
  *)
  and infer_lambda hypothesis (name, body, implicit, c) =
    let reference = ref None in
    let bound' = hole ~r:reference name in
    let hypothesis = add_signature hypothesis (name, bound') in
    let body', proof = hypothesis |- body => () in
    proof_from_option
      ( body'
      <&> fun body' ->
      fold_right const
        ( !reference
        <&> fun value -> (Some (pi ~implicit ~c name value body'), [ proof ]) )
        ( Some
            (pi ~implicit:true name (kind 0)
               (pi ~implicit ~c name bound' body') )
          (* Why type0 / See type in type? *)
        , [ proof ] ) )
      [ proof ]

  (*
    Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M
    ----------------------------      ----------------------------
    Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]
  *)
  and infer_apply hypothesis (abstraction, argument, implicit, _c) =
    let pi, proof = hypothesis |- abstraction => () in
    proof_from_option
      ( pi
      >>= fold_opt ~pi:return
      <&> fun (n, bound, body, implicit', _c') ->
      if implicit = implicit'
      then
        ( return (substitute n argument body)
        , [ hypothesis |- argument <= bound ] )
      else (None, [ proof; failure None ]) )
      [ proof ]

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Σ(x:M).N : T
  *)
  and infer_sigma hypothesis (name, bound, body, _c) =
    let bound', proof = hypothesis |- bound => () in
    let body', proof' = add_signature hypothesis (name, bound) |- body => () in
    (bound' >>= const body', [ proof; proof' ])

  (*
    Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    --------------------------
    Γ ⊢ A,B : Σ(x:M).N
  *)
  and infer_pair hypothesis (lhd, rhd, _c) =
    let left, proof = hypothesis |- lhd => () in
    let right, proof' = hypothesis |- rhd => () in
    ( (left >>= fun left -> right <&> fun right -> sigma "_" left right)
    , [ proof; proof' ] )

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------
    Γ ⊢ fst p : M
  *)
  and infer_fst hypothesis (term, _c) =
    let sigma, proof = hypothesis |- term => () in
    proof_from_option
      ~reason:(return "Waiting for a sigma")
      ( sigma
      >>= fold_opt ~sigma:return
      <&> fun (_, bound, _, _) -> (return bound, [ proof ]) )
      [ proof ]

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------------
    Γ ⊢ snd p : N[x=fst p]
  *)
  and infer_snd hypothesis (term, _c) =
    let sigma, proof = hypothesis |- term => () in
    proof_from_option
      ~reason:(return "Waiting for a sigma")
      ( sigma
      >>= fold_opt ~sigma:return
      <&> fun (n, _, body, _) ->
      (return (substitute n (fst term) body), [ proof ]) )
      [ proof ]

  (*
    Γ ⊢ A : T   Γ ⊢ B : T
    ---------------------
    Γ ⊢ A + B : T
  *)
  and infer_sum hypothesis (lhd, rhd, _c) =
    let term, proof = hypothesis |- lhd => () in
    proof_from_option
      (term <&> fun term -> (return term, [ hypothesis |- rhd <= term ]))
      [ proof ]

  (*
    Γ ⊢ A : M
    -----------------
    Γ ⊢ inl A : M + N
  *)
  and infer_inl hypothesis (term, c) =
    let term, proof = hypothesis |- term => () in
    proof_from_option
      ( term
      <&> fun term ->
      let var, _hypothesis = fresh_variable hypothesis "_" in
      (return (sum term (hole ~c var)), [ proof ]) )
      [ proof ]

  (*
    Γ ⊢ A : N
    -----------------
    Γ ⊢ inr A : M + N
  *)
  and infer_inr hypothesis (term, c) =
    let term, proof = hypothesis |- term => () in
    proof_from_option
      ( term
      <&> fun term ->
      let var, _hypothesis = fresh_variable hypothesis "_" in
      (return (sum (hole ~c var) term), [ proof ]) )
      [ proof ]

  (*
    Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C   Γ ⊢ r : Π(_:B).T
    ---------------------------------------------------
    Γ ⊢ case a l r : C
  *)
  and infer_case hypothesis (term, left, right, _c) =
    let term, proof = hypothesis |- term => () in
    proof_from_option
      ( term
      >>= fold_opt ~sum:return
      <&> fun (lhd, rhd, c) ->
      let var, hypothesis = fresh_variable hypothesis "_" in
      let hole = hole ~c var in
      ( return hole
      , [
          proof
        ; hypothesis |- left <= arrow lhd hole
        ; hypothesis |- right <= arrow rhd hole
        ] ) )
      [ proof ]

  (*
    Γ,x : T ⊢ A : T
    ---------------
    Γ ⊢ μ(x).A : T
  *)
  and infer_mu hypothesis (name, body, c) =
    let var, hypothesis = fresh_variable hypothesis name in
    let hypothesis = add_signature hypothesis (name, hole ~c var) in
    let term, proof = hypothesis |- body => () in
    (term, [ proof ])

  (*
    Γ ⊢ A : N[x=μ(x).N]
    -------------------
    Γ ⊢ fold A : μ(x).N
  *)
  and infer_fold _hypothesis (_term, _c) =
    (None, [ failure @@ return "Cannot infer fold" ])

  (*
    Γ ⊢ A : μ(x).N
    --------------------------
    Γ ⊢ unfold A : N[x=μ(x).N]
  *)
  and infer_unfold hypothesis (term, _c) =
    let term, proof = hypothesis |- term => () in
    proof_from_option
      ( term
      >>= fold_opt ~mu:return
      <&> fun (n, body, c) ->
      (return (substitute n (mu ~c n body) body), [ proof ]) )
      [ proof ]

  (*
    Γ ⊢                   Γ, x : T ⊢ U : T
    -----------------     -------------------
    Γ, x : T ⊢ ?x : T     Γ, x : T ⊢ ?x=U : T
  *)
  and infer_hole hypothesis (name, _value, _c) =
    proof_from_option
      ~reason:(return "Unbound variable")
      (get_signature hypothesis name <&> fun t -> (return t, []))
      []

  and infer_type hypothesis term =
    let term', proofs =
      fold ~kind:(infer_kind hypothesis) ~int:(infer_int hypothesis)
        ~char:(infer_char hypothesis) ~string:(infer_string hypothesis)
        ~id:(infer_id hypothesis) ~pi:(infer_pi hypothesis)
        ~lambda:(infer_lambda hypothesis) ~apply:(infer_apply hypothesis)
        ~sigma:(infer_sigma hypothesis) ~pair:(infer_pair hypothesis)
        ~fst:(infer_fst hypothesis) ~snd:(infer_snd hypothesis)
        ~sum:(infer_sum hypothesis) ~inl:(infer_inl hypothesis)
        ~inr:(infer_inr hypothesis) ~case:(infer_case hypothesis)
        ~mu:(infer_mu hypothesis) ~fold:(infer_fold hypothesis)
        ~unfold:(infer_unfold hypothesis) ~hole:(infer_hole hypothesis) term
    in
    (term' <&> reduce hypothesis, infer term term' proofs)

  and ( => ) (hypothesis, term) () = infer_type hypothesis term
end
