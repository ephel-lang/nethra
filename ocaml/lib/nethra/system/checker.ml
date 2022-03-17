module Impl (Infer : Specs.Infer) = struct
  include Goal
  open Nethra_ast.Term.Builders
  open Nethra_ast.Term.Catamorphism
  open Nethra_ast.Proof
  open Nethra_ast.Proof.Builders
  open Nethra_ast.Bindings.Access
  open Substitution
  open Reduction
  open Congruence
  open Infer

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let check_kind bindings term' (level, c) =
    let term, proof = bindings |- kind ~c level <:?> () in
    match term with
    | Some term -> [ proof; bindings |- term =?= term' ]
    | None -> [ proof; failure None ]

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let check_int bindings term' (value, c) =
    let term, proof = bindings |- int ~c value <:?> () in
    match term with
    | Some term -> [ proof; bindings |- term =?= term' ]
    | None -> [ proof; failure None ]

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let check_char bindings term' (value, c) =
    let term, proof = bindings |- char ~c value <:?> () in
    match term with
    | Some term -> [ proof; bindings |- term =?= term' ]
    | None -> [ proof; failure None ]

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let check_string bindings term' (value, c) =
    let term, proof = bindings |- string ~c value <:?> () in
    match term with
    | Some term -> [ proof; bindings |- term =?= term' ]
    | None -> [ proof; failure None ]

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let check_id bindings term' (name, initial, c) =
    let term, proof = bindings |- id ~c ~initial name <:?> () in
    match term with
    | Some term -> [ proof; bindings |- term =?= term' ]
    | None -> [ proof; failure @@ Some ("Unbound variable " ^ name) ]

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
    match fold_opt ~pi:(fun p -> Some p) term' with
    | Some (name', bound', body', implicit', _c') ->
      if implicit = implicit'
      then
        let var, bindings = fresh_variable bindings name' in
        let body' = substitute name' (id ~initial:(Some name') var) body' in
        let body = substitute name (id ~initial:(Some name) var) body in
        [ add_signature bindings (var, bound') |- body <?:> body' ]
      else [ failure None ]
    | None -> [ failure @@ Some "Waiting for a Pi term" ]

  and check_apply bindings _term' (abstraction, _argument, _implicit, _c) =
    match bindings |- abstraction <:?> () with
    | Some _t, _proof -> [ failure @@ Some "TODO" ]
    | None, proof -> [ proof; failure None ]

  and check_sigma bindings term' (name, bound, body, _c) =
    let bindings = add_signature bindings (name, bound) in
    [ bindings |- body <?:> term' ]

  and check_pair _bindings _term' (_lhd, _rhd, _c) = [ failure @@ Some "TODO" ]
  and check_fst _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]
  and check_snd _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]
  and check_sum _bindings _term' (_lhd, _rhd, _c) = [ failure @@ Some "TODO" ]
  and check_inl _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]
  and check_inr _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]

  and check_case _bindings _term' (_term, _left, _right, _c) =
    [ failure @@ Some "TODO" ]

  and check_mu _bindings _term' (_name, _body, _c) = [ failure @@ Some "TODO" ]
  and check_fold _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]
  and check_unfold _bindings _term' (_term, _c) = [ failure @@ Some "TODO" ]

  and check_hole _bindings _term' (_name, _value, _c) =
    [ failure @@ Some "TODO" ]

  (*
    Γ ⊢ λ{x}.B : Π{x:A}.T   B ≠ λ{y}.C
    ----------------------------------
    Γ ⊢ B : Π{x:A}.T
  *)
  and implicit bindings term term' =
    let implicit_lambda = fold_opt ~lambda:(fun (_, _, i, _) -> Some i) term in
    let implicit_pi = fold_opt ~pi:(fun (n, _, _, i, _) -> Some (n, i)) term' in
    match (implicit_lambda, implicit_pi) with
    | Some true, _ -> [ failure None ]
    | _, Some (n, true) ->
      [ bindings |- lambda ~implicit:true n term <?:> term' ]
    | _ -> [ failure None ]

  (*
    Γ ⊢ t : Type_i
    ------------------
    Γ ⊢ t : Type_{i+1}
  *)
  and type_level bindings term term' =
    let level = fold_opt ~kind:(fun t -> Some t) term' in
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
    let tactics = [ nominal; implicit; type_level ] in
    (* Either can be uses here + Foldable *)
    let success, failures =
      List.fold_left
        (fun (success, failures) tactic ->
          match success with
          | Some _ -> (success, [])
          | None ->
            let proof = check term term' (tactic bindings term term') in
            if is_success proof
            then (Some proof, [])
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
