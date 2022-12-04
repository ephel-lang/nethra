(* Reference management in holes should be replaced thanks to a
   state monad embedding the hypothesis *)

module Impl (Theory : Specs.Theory) = struct
  include Judgment
  open Stdlib.Fun
  open Preface.Option.Monad
  open Preface.Option.Foldable
  open Nethra_lang_ast.Term.Construct
  open Nethra_lang_ast.Term.Destruct
  open Nethra_lang_ast.Proof.Construct
  open Nethra_lang_ast.Hypothesis.Access
  open Nethra_lang_basic.Substitution
  open Reduction

  let proof_from_option ?(proofs = []) ?(reason = None) o =
    fold_right const o (proofs @ [ failure reason ])

  let equivalent_kind _hypothesis term' (level, _) =
    proof_from_option
      ( fold_opt ~kind:return term'
      >>= fun (level', _) ->
      if Theory.type_in_type || level = level' then Some [] else None )
  (* Type in Type *)

  let equivalent_int _hypothesis term' (value, _) =
    proof_from_option
      ( fold_opt ~int:return term'
      >>= fun (value', _) -> if value = value' then Some [] else None )

  let equivalent_char _hypothesis term' (value, _) =
    proof_from_option
      ( fold_opt ~char:return term'
      >>= fun (value', _) -> if value = value' then Some [] else None )

  let equivalent_string _hypothesis term' (value, _) =
    proof_from_option
      ( fold_opt ~string:return term'
      >>= fun (value', _) -> if value = value' then Some [] else None )

  let equivalent_id _hypothesis term' (name, _, _) =
    proof_from_option
      ( fold_opt ~id:return term'
      >>= fun (name', _, _) -> if name = name' then Some [] else None )

  let rec equivalent_pi hypothesis term' (name, bound, body, implicit, c) =
    proof_from_option
      ( fold_opt ~pi:return term'
      >>= fun (name', bound', body', implicit', c') ->
      if implicit' = implicit
      then
        let var, hypothesis = fresh_variable hypothesis name in
        let body = substitute name (id ~c ~initial:(Some name) var) body
        and body' =
          substitute name' (id ~c:c' ~initial:(Some name') var) body'
        in
        Some [ hypothesis |- bound =?= bound'; hypothesis |- body =?= body' ]
      else None )

  and equivalent_lambda hypothesis term' (name, body, implicit, c) =
    proof_from_option
      ( fold_opt ~lambda:return term'
      >>= fun (name', body', implicit', c') ->
      if implicit' = implicit
      then
        let var, hypothesis = fresh_variable hypothesis name in
        let body = substitute name (id ~c ~initial:(Some name) var) body
        and body' =
          substitute name' (id ~c:c' ~initial:(Some name') var) body'
        in
        Some [ hypothesis |- body =?= body' ]
      else None )

  and equivalent_apply hypothesis term' (abstraction, argument, implicit, _) =
    proof_from_option
      ( fold_opt ~apply:return term'
      >>= fun (abstraction', argument', implicit', _) ->
      if implicit' = implicit
      then
        Some
          [
            hypothesis |- abstraction =?= abstraction'
          ; hypothesis |- argument =?= argument'
          ]
      else None )

  and equivalent_let_binding hypothesis term' (name, arg, body, _c) =
    proof_from_option
      ( fold_opt ~let_binding:return term'
      <&> fun (name', arg', body', _c') ->
      let body = substitute name arg body
      and body' = substitute name' arg' body' in
      [ hypothesis |- body =?= body' ] )

  and equivalent_sigma hypothesis term' (name, bound, body, c) =
    proof_from_option
      ( fold_opt ~sigma:return term'
      <&> fun (name', bound', body', c') ->
      let var, hypothesis = fresh_variable hypothesis name in
      let body = substitute name (id ~c ~initial:(Some name) var) body
      and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
      [ hypothesis |- bound =?= bound'; hypothesis |- body =?= body' ] )

  and equivalent_pair hypothesis term' (lhd, rhd, _c) =
    proof_from_option
      ( fold_opt ~pair:return term'
      <&> fun (lhd', rhd', _) ->
      [ hypothesis |- lhd =?= lhd'; hypothesis |- rhd =?= rhd' ] )

  and equivalent_fst hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~fst:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_snd hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~snd:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_sum hypothesis term' (lhd, rhd, _c) =
    proof_from_option
      ( fold_opt ~sum:return term'
      <&> fun (lhd', rhd', _) ->
      [ hypothesis |- lhd =?= lhd'; hypothesis |- rhd =?= rhd' ] )

  and equivalent_inl hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~inl:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_inr hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~inr:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_case hypothesis term' (term, left, right, _c) =
    proof_from_option
      ( fold_opt ~case:return term'
      <&> fun (term', left', right', _c') ->
      [
        hypothesis |- term =?= term'
      ; hypothesis |- left =?= left'
      ; hypothesis |- right =?= right'
      ] )

  and equivalent_mu hypothesis term' (name, kind, body, c) =
    proof_from_option
      ( fold_opt ~mu:return term'
      <&> fun (name', kind', body', c') ->
      let var, hypothesis = fresh_variable hypothesis name in
      let body = substitute name (id ~c ~initial:(Some name) var) body
      and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
      [ hypothesis |- kind =?= kind'; hypothesis |- body =?= body' ] )

  and equivalent_fold hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~fold:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_unfold hypothesis term' (term, _c) =
    proof_from_option
      ( fold_opt ~unfold:return term'
      <&> fun (term', _) -> [ hypothesis |- term =?= term' ] )

  and equivalent_hole hypothesis term' (name, reference, _c) =
    match !reference with
    | Some term -> [ hypothesis |- term =?= term' ]
    | None -> (
      match fold_opt ~hole:return term' with
      | Some (name', _, _) when name = name' -> []
      | _ ->
        let () = reference := Some term' in
        [] )

  and equivalent_annotation hypothesis term' (term, kind, _c) =
    proof_from_option
      ( fold_opt ~pair:return term'
      <&> fun (term', kind', _) ->
      [ hypothesis |- term =?= term'; hypothesis |- kind =?= kind' ] )

  and equivalent_equals _hypothesis _term' (_lhd, _rhd, _c) = [ failure None ]

  and equivalent_refl _hypothesis term' _c =
    proof_from_option (fold_opt ~refl:return term' <&> fun _ -> [])

  and equivalent_subst _hypothesis _term' (_lhd, _rhd, _c) = [ failure None ]

  and equivalent_record hypothesis l' l =
    let proofs =
      if List.length l = List.length l'
      then []
      else [ failure (Some "Record should have the same size") ]
    in
    List.fold_right
      (fun (n, e) p ->
        proof_from_option
          ~reason:(Some (n ^ " not found"))
          ~proofs:p
          ( List.find_opt (fun (n', _) -> n' = n) l'
          <&> (fun (_, t) -> t)
          <&> fun t -> (hypothesis |- e =?= t) :: p ) )
      l proofs

  and equivalent_record_sig hypothesis term' (l, _c) =
    proof_from_option
      ~reason:(return "Waiting for a record signature")
      ( fold_opt ~record_sig:return term'
      <&> fun (l', _c) -> equivalent_record hypothesis l' l )

  and equivalent_record_val hypothesis term' (l, _c) =
    proof_from_option
      ~reason:(return "Waiting for a record value")
      ( fold_opt ~record_val:return term'
      <&> fun (l', _c) -> equivalent_record hypothesis l' l )

  and equivalent_access _hypothesis _term' (_t, _n, _c) = [ failure None ]

  and equivalent_terms hypothesis term term' =
    let term = reduce hypothesis term
    and term' = reduce hypothesis term' in
    let term, term' =
      fold_right const
        (fold_opt ~hole:return term' <&> fun _ -> (term', term))
        (term, term')
    in
    let proofs =
      fold
        ~kind:(equivalent_kind hypothesis term')
        ~int:(equivalent_int hypothesis term')
        ~char:(equivalent_char hypothesis term')
        ~string:(equivalent_string hypothesis term')
        ~id:(equivalent_id hypothesis term')
        ~pi:(equivalent_pi hypothesis term')
        ~lambda:(equivalent_lambda hypothesis term')
        ~apply:(equivalent_apply hypothesis term')
        ~let_binding:(equivalent_let_binding hypothesis term')
        ~sigma:(equivalent_sigma hypothesis term')
        ~pair:(equivalent_pair hypothesis term')
        ~fst:(equivalent_fst hypothesis term')
        ~snd:(equivalent_snd hypothesis term')
        ~sum:(equivalent_sum hypothesis term')
        ~inl:(equivalent_inl hypothesis term')
        ~inr:(equivalent_inr hypothesis term')
        ~case:(equivalent_case hypothesis term')
        ~mu:(equivalent_mu hypothesis term')
        ~fold:(equivalent_fold hypothesis term')
        ~unfold:(equivalent_unfold hypothesis term')
        ~hole:(equivalent_hole hypothesis term')
        ~annotation:(equivalent_annotation hypothesis term')
        ~equals:(equivalent_equals hypothesis term')
        ~refl:(equivalent_refl hypothesis term')
        ~subst:(equivalent_subst hypothesis term')
        ~record_sig:(equivalent_record_sig hypothesis term')
        ~record_val:(equivalent_record_val hypothesis term')
        ~access:(equivalent_access hypothesis term')
        term
    in
    equivalent hypothesis term term' proofs

  and ( =?= ) (hypothesis, term) term' = equivalent_terms hypothesis term term'
end