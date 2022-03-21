module Impl (Infer : Specs.Infer) = struct
  include Goal
  open Stdlib.Fun
  open Preface.Option.Monad
  open Preface.Option.Foldable
  open Nethra_ast.Term.Builders
  open Nethra_ast.Term.Catamorphism
  open Nethra_ast.Proof
  open Nethra_ast.Proof.Builders
  open Nethra_ast.Bindings.Access
  open Reduction
  open Substitution
  open Congruence
  open Infer

  let proof_from_option ?(reason = None) o =
    fold_right const o [ failure reason ]

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let check_kind bindings term' (level, c) =
    let term, proof = bindings |- kind ~c level <:?> () in
    proof_from_option
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let check_int bindings term' (value, c) =
    let term, proof = bindings |- int ~c value <:?> () in
    proof_from_option
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let check_char bindings term' (value, c) =
    let term, proof = bindings |- char ~c value <:?> () in
    proof_from_option
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let check_string bindings term' (value, c) =
    let term, proof = bindings |- string ~c value <:?> () in
    proof_from_option
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let check_id bindings term' (name, initial, c) =
    let term, proof = bindings |- id ~c ~initial name <:?> () in
    proof_from_option
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Π(x:M).N : T

  *)
  let rec check_pi bindings term' (name, bound, body, _implicit, _c) =
    [
      Stdlib.snd (bindings |- bound <:?> ())
    ; add_signature bindings (name, bound) |- body <?:> term'
    ]

  (*
    Γ, x : A ⊢ B : T          Γ, x : A ⊢ B : T
    ---------------------     ---------------------
    Γ ⊢ λ(x).B : Π(x:A).T     Γ ⊢ λ{x}.B : Π{x:A}.T
  *)
  and check_lambda bindings term' (name, body, implicit, _c) =
    proof_from_option
      ~reason:(return "Waiting for a Pi term")
      ( fold_opt ~pi:(fun p -> return p) term'
      <&> fun (name', bound', body', implicit', _c') ->
      if implicit = implicit'
      then
        let var, bindings = fresh_variable bindings name' in
        let body' = substitute name' (id ~initial:(return name') var) body'
        and body = substitute name (id ~initial:(return name) var) body in
        [ add_signature bindings (var, bound') |- body <?:> body' ]
      else [ failure None ] )

  (*
    Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M
    ----------------------------      ----------------------------
    Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]
  *)
  and check_apply bindings term' (abstraction, argument, implicit, _c) =
    let term, proof = bindings |- abstraction <:?> () in
    proof_from_option
      ( term
      >>= fold_opt ~pi:(fun t -> return t)
      >>= fun (name, bound, body, implicit', _) ->
      if implicit = implicit'
      then
        return
          [
            proof
          ; bindings |- argument <?:> bound
          ; bindings |- substitute name argument body =?= term'
          ]
      else None )

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Σ(x:M).N : T
  *)
  and check_sigma bindings term' (name, bound, body, _c) =
    [
      Stdlib.snd (bindings |- bound <:?> ())
    ; add_signature bindings (name, bound) |- body <?:> term'
    ]

  (*
    Γ ⊢ A : M   Γ ⊢ B : N[x=A]
    --------------------------
    Γ ⊢ A,B : Σ(x:M).N
  *)
  and check_pair bindings term' (lhd, rhd, _c) =
    proof_from_option
      ~reason:(return "Waiting for a sigma")
      ( fold_opt ~sigma:(fun t -> return t) term'
      <&> fun (n', bound', body', _c') ->
      [
        bindings |- lhd <?:> bound'
      ; bindings |- rhd <?:> substitute n' lhd body'
      ] )

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------
    Γ ⊢ fst p : M
  *)
  and check_fst bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    proof_from_option
      ( term''
      >>= fold_opt ~sigma:(fun t -> return t)
      <&> fun (_, bound, _, _) -> [ proof; bindings |- bound =?= term' ] )

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------------
    Γ ⊢ snd p : N[x=fst p]
    *)
  and check_snd bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    proof_from_option
      ( term''
      >>= fold_opt ~sigma:(fun t -> return t)
      <&> fun (n, _, body, _) ->
      [ proof; bindings |- substitute n (fst term) body =?= term' ] )

  (*
    Γ ⊢ A : T   Γ ⊢ B : T
    ---------------------
    Γ ⊢ A + B : T
  *)
  and check_sum bindings term' (lhd, rhd, _c) =
    [ bindings |- lhd <?:> term'; bindings |- rhd <?:> term' ]

  (*
    Γ ⊢ A : M
    -----------------
    Γ ⊢ inl A : M + N
  *)
  and check_inl bindings term' (term, _c) =
    proof_from_option
      ( fold_opt ~sum:(fun t -> return t) term'
      <&> fun (lhd, _, _) -> [ bindings |- term <?:> lhd ] )

  (*
    Γ ⊢ A : N
    -----------------
    Γ ⊢ inr A : M + N
  *)
  and check_inr bindings term' (term, _c) =
    proof_from_option
      ( fold_opt ~sum:(fun t -> return t) term'
      <&> fun (_, rhd, _) -> [ bindings |- term <?:> rhd ] )

  (*
    Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C   Γ ⊢ r : Π(_:B).T
    ---------------------------------------------------
    Γ ⊢ case a l r : C
  *)
  and check_case bindings term' (term, left, right, _c) =
    let term'', proof = bindings |- term <:?> () in
    proof_from_option
      ( term''
      >>= fold_opt ~sum:(fun t -> return t)
      <&> fun (lhd, rhd, _) ->
      [
        proof
      ; bindings |- left <?:> pi "_" lhd term'
      ; bindings |- right <?:> pi "_" rhd term'
      ] )

  (*
    Γ,x : T ⊢ A : T
    ---------------
    Γ ⊢ μ(x).A : T
  *)
  and check_mu bindings term' (name, body, _c) =
    [ add_signature bindings (name, body) |- body <?:> term' ]

  (*
    Γ ⊢ A : N[x=μ(x).N]
    -------------------
    Γ ⊢ fold A : μ(x).N
  *)
  and check_fold bindings term' (term, _c) =
    proof_from_option
      ( fold_opt ~mu:(fun t -> return t) term'
      <&> fun (name', term'', _) ->
      [ bindings |- term <?:> substitute name' term' term'' ] )

  (*
    Γ ⊢ A : μ(x).N
    --------------------------
    Γ ⊢ unfold A : N[x=μ(x).N]
  *)
  and check_unfold bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    proof_from_option
      ( term''
      >>= fold_opt ~mu:(fun t -> return t)
      <&> fun (name', term'', _) ->
      [ proof; bindings |- term' =?= substitute name' term'' term'' ] )

  (*
    Γ ⊢                   Γ, x : T ⊢ U : T
    -----------------     -------------------
    Γ, x : T ⊢ ?x : T     Γ, x : T ⊢ ?x=U : T
  *)
  and check_hole bindings term' (name, reference, _c) =
    proof_from_option
      ~reason:(return "Unbound variable")
      ( get_signature bindings name
      <&> fun term ->
      [ bindings |- term =?= term' ]
      @ fold_right const
          (!reference <&> fun term -> [ bindings |- term <?:> term' ])
          [] )

  (*
    Γ ⊢ λ{x}.B : Π{x:A}.T   B ≠ λ{y}.C
    ----------------------------------
    Γ ⊢ B : Π{x:A}.T
  *)
  and implicit_argument bindings term term' =
    proof_from_option
      (let implicit_lambda =
         fold_right const
           (fold_opt ~lambda:(fun (_, _, i, _) -> return i) term)
           false
       in
       fold_opt ~pi:(fun (n, _, _, i, _) -> return (n, i)) term'
       <&> fun (n, implicit_pi) ->
       if (not implicit_lambda) && implicit_pi
       then [ bindings |- lambda ~implicit:true n term <?:> term' ]
       else [ failure None ] )

  (*
    Γ ⊢ f : Π{x:M}.C   Γ, v:M ⊢ f {v} e : N
    ---------------------------------------
    Γ ⊢ f e : N
  *)
  and implicit_parameter bindings term term' =
    proof_from_option
      ( fold_opt ~apply:(fun t -> return t) term
      <&> fun (abstraction, argument, implicit_apply, _) ->
      if implicit_apply
      then [ failure None ]
      else
        let term'', _ = bindings |- abstraction <:?> () in
        proof_from_option
          ( term''
          >>= fold_opt ~pi:(fun t -> return t)
          <&> fun (n, bound, _, implicit_pi, _) ->
          if implicit_pi
          then
            let var, bindings = fresh_variable bindings n in
            let term =
              apply (apply ~implicit:true abstraction (hole var)) argument
            in
            [ add_signature bindings (var, bound) |- term <?:> term' ]
          else [ failure None ] ) )

  (*
    Γ ⊢ t : Type_i
    ------------------
    Γ ⊢ t : Type_{i+1}
  *)
  and type_level bindings term term' =
    proof_from_option
      ( fold_opt ~kind:(fun t -> return t) term'
      <&> fun (level, c) ->
      if level > 0
      then [ bindings |- term <?:> kind ~c (level - 1) ]
      else [ failure None ] )

  and nominal bindings term term' =
    fold
      ~kind:(check_kind bindings term')
      ~int:(check_int bindings term')
      ~char:(check_char bindings term')
      ~string:(check_string bindings term')
      ~id:(check_id bindings term') ~pi:(check_pi bindings term')
      ~lambda:(check_lambda bindings term')
      ~apply:(check_apply bindings term')
      ~sigma:(check_sigma bindings term')
      ~pair:(check_pair bindings term')
      ~fst:(check_fst bindings term') ~snd:(check_snd bindings term')
      ~sum:(check_sum bindings term') ~inl:(check_inl bindings term')
      ~inr:(check_inr bindings term')
      ~case:(check_case bindings term')
      ~mu:(check_mu bindings term')
      ~fold:(check_fold bindings term')
      ~unfold:(check_unfold bindings term')
      ~hole:(check_hole bindings term')
      term

  (* type checker main entrypoint *)
  and check_type bindings term term' =
    let term' = reduce bindings term' in
    let tactics =
      [ nominal; implicit_argument; implicit_parameter; type_level ]
    in
    (* Either can be uses here + Foldable *)
    let success, failures =
      List.fold_left
        (fun (success, failures) tactic ->
          match success with
          | Some _ -> (success, [])
          | None ->
            let proof = check term term' (tactic bindings term term') in
            if is_success proof
            then (return proof, [])
            else (None, proof :: failures) )
        (None, []) tactics
    in
    match success with
    | Some proof -> proof
    | None ->
      List.fold_left
        (fun p p' -> if size p > size p' then p else p')
        (failure None) failures

  and ( <?:> ) (bindings, term) term' = check_type bindings term term'
end
