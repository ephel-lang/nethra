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
  open Substitution
  open Congruence
  open Infer
  open Reduction

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let check_kind bindings term' (level, c) =
    let term, proof = bindings |- kind ~c level <:?> () in
    fold_right const
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])
      [ proof; failure None ]

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let check_int bindings term' (value, c) =
    let term, proof = bindings |- int ~c value <:?> () in
    fold_right const
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])
      [ proof; failure None ]

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let check_char bindings term' (value, c) =
    let term, proof = bindings |- char ~c value <:?> () in
    fold_right const
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])
      [ proof; failure None ]

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let check_string bindings term' (value, c) =
    let term, proof = bindings |- string ~c value <:?> () in
    fold_right const
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])
      [ proof; failure None ]

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let check_id bindings term' (name, initial, c) =
    let term, proof = bindings |- id ~c ~initial name <:?> () in
    fold_right const
      (term <&> fun term -> [ proof; bindings |- term =?= term' ])
      [ proof; failure None ]

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
    fold_right const
      ( fold_opt ~pi:(fun p -> return p) term'
      <&> fun (name', bound', body', implicit', _c') ->
      if implicit = implicit'
      then
        let var, bindings = fresh_variable bindings name' in
        let body' = substitute name' (id ~initial:(return name') var) body'
        and body = substitute name (id ~initial:(return name) var) body in
        [ add_signature bindings (var, bound') |- body <?:> body' ]
      else [ failure None ] )
      [ failure @@ return "Waiting for a Pi term" ]

  (*
    Γ ⊢ f : Π(x:M).N   Γ ⊢ e : M      Γ ⊢ f : Π{x:M}.N   Γ ⊢ e : M
    ----------------------------      ----------------------------
    Γ ⊢ f e : N[x=e]                  Γ ⊢ f {e} : N[x=e]
  *)
  and check_apply bindings term' (abstraction, argument, implicit, _c) =
    let term, proof = bindings |- abstraction <:?> () in
    fold_right const
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
      [ proof; failure None ]

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
    fold_right const
      ( fold_opt ~sigma:(fun t -> return t) term'
      <&> fun (n', bound', body', _c') ->
      [
        bindings |- lhd <?:> bound'
      ; bindings |- rhd <?:> substitute n' lhd body'
      ] )
      [ failure (return "Waiting for a sigma") ]

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------
    Γ ⊢ fst p : M
  *)
  and check_fst bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    fold_right const
      ( term''
      >>= fold_opt ~sigma:(fun t -> return t)
      <&> fun (_, bound, _, _) -> [ proof; bindings |- bound =?= term' ] )
      [ proof; failure None ]

  (*
    Γ ⊢ p : Σ(x:M).N
    ----------------------
    Γ ⊢ snd p : N[x=fst p]
    *)
  and check_snd bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    fold_right const
      ( term''
      >>= fold_opt ~sigma:(fun t -> return t)
      <&> fun (n, _, body, _) ->
      [ proof; bindings |- substitute n (fst term) body =?= term' ] )
      [ proof; failure None ]

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
    fold_right const
      ( fold_opt ~sum:(fun t -> return t) term'
      <&> fun (lhd, _, _) -> [ bindings |- term <?:> lhd ] )
      [ failure None ]

  (*
    Γ ⊢ A : N
    -----------------
    Γ ⊢ inr A : M + N
  *)
  and check_inr bindings term' (term, _c) =
    fold_right const
      ( fold_opt ~sum:(fun t -> return t) term'
      <&> fun (_, rhd, _) -> [ bindings |- term <?:> rhd ] )
      [ failure None ]

  (*
    Γ ⊢ a : A + B   Γ ⊢ l : Π(_:A).C   Γ ⊢ r : Π(_:B).T
    ---------------------------------------------------
    Γ ⊢ case a l r : C
  *)
  and check_case bindings term' (term, left, right, _c) =
    let term'', proof = bindings |- term <:?> () in
    fold_right const
      ( term''
      >>= fold_opt ~sum:(fun t -> return t)
      <&> fun (lhd, rhd, _) ->
      [
        proof
      ; bindings |- left <?:> pi "_" lhd term'
      ; bindings |- right <?:> pi "_" rhd term'
      ] )
      [ proof; failure None ]

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
    fold_right const
      ( fold_opt ~mu:(fun t -> return t) term'
      <&> fun (name', term'', _) ->
      [ bindings |- term <?:> substitute name' term' term'' ] )
      [ failure None ]

  (*
    Γ ⊢ A : μ(x).N
    --------------------------
    Γ ⊢ unfold A : N[x=μ(x).N]
  *)
  and check_unfold bindings term' (term, _c) =
    let term'', proof = bindings |- term <:?> () in
    fold_right const
      ( term''
      >>= fold_opt ~mu:(fun t -> return t)
      <&> fun (name', term'', _) ->
      [ proof; bindings |- term' =?= substitute name' term'' term'' ] )
      [ proof; failure None ]

  and check_hole _bindings _term' (_name, _value, _c) =
    [ failure @@ return "TODO" ]

  (*
    Γ ⊢ λ{x}.B : Π{x:A}.T   B ≠ λ{y}.C
    ----------------------------------
    Γ ⊢ B : Π{x:A}.T
  *)
  and implicit_argument bindings term term' =
    let implicit_lambda =
      fold_opt ~lambda:(fun (_, _, i, _) -> return i) term
    in
    let implicit_pi =
      fold_opt ~pi:(fun (n, _, _, i, _) -> return (n, i)) term'
    in
    match (implicit_lambda, implicit_pi) with
    | Some true, _ -> [ failure None ]
    | _, Some (n, true) ->
      [ bindings |- lambda ~implicit:true n term <?:> term' ]
    | _ -> [ failure None ]

  (*
    Γ ⊢ f : Π{x:M}.N   Γ, v:M ⊢ f {v} e : N
    ---------------------------------------
    Γ ⊢ f e : N
  *)
  and implicit_parameter _bindings _term _term' = [ failure @@ return "TODO" ]

  (*
    Γ ⊢ t : Type_i
    ------------------
    Γ ⊢ t : Type_{i+1}
  *)
  and type_level bindings term term' =
    let level = fold_opt ~kind:(fun t -> return t) term' in
    match level with
    | Some (level, c) when level > 0 ->
      [ bindings |- term <?:> kind ~c (level - 1) ]
    | _ -> [ failure None ]

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
