open Nethra_ast.Ast.Term.Builders
open Nethra_ast.Ast.Term.Catamorphism
open Nethra_ast.Ast.Proof.Builders
open Nethra_ast.Ast.Bindings.Access
open Reduction
open Substitution
include Goal

(* Reference management in holes should be replaced thanks to a
   state monad embedding the bindings *)

let congruent_kind term' (level, _) =
  match fold_opt ~kind:(fun t -> Some t) term' with
  | Some (level', _) when level = level' -> []
  | _ -> [ failure None ]

let congruent_int term' (value, _) =
  match fold_opt ~int:(fun t -> Some t) term' with
  | Some (value', _) when value = value' -> []
  | _ -> [ failure None ]

let congruent_char term' (value, _) =
  match fold_opt ~char:(fun t -> Some t) term' with
  | Some (value', _) when value = value' -> []
  | _ -> [ failure None ]

let congruent_string term' (value, _) =
  match fold_opt ~string:(fun t -> Some t) term' with
  | Some (value', _) when value = value' -> []
  | _ -> [ failure None ]

let congruent_id term' (name, _, _) =
  match fold_opt ~id:(fun t -> Some t) term' with
  | Some (name', _, _) when name = name' -> []
  | _ -> [ failure None ]

let rec congruent_pi bindings term' (name, bound, body, implicit, c) =
  match fold_opt ~pi:(fun t -> Some t) term' with
  | Some (name', bound', body', implicit', c') when implicit' = implicit ->
    let var, bindings = fresh_variable bindings name in
    let body = substitute name (id ~c ~initial:(Some name) var) body
    and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
    [ bindings |- bound =?= bound'; bindings |- body =?= body' ]
  | _ -> [ failure None ]

and congruent_lambda bindings term' (name, body, implicit, c) =
  match fold_opt ~lambda:(fun t -> Some t) term' with
  | Some (name', body', implicit', c') when implicit' = implicit ->
    let var, bindings = fresh_variable bindings name in
    let body = substitute name (id ~c ~initial:(Some name) var) body
    and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
    [ bindings |- body =?= body' ]
  | _ -> [ failure None ]

and congruent_apply bindings term' (abstraction, argument, implicit, _) =
  match fold_opt ~apply:(fun t -> Some t) term' with
  | Some (abstraction', argument', implicit', _) when implicit' = implicit ->
    [
      bindings |- abstraction =?= abstraction'
    ; bindings |- argument =?= argument'
    ]
  | _ -> [ failure None ]

and congruent_sigma bindings term' (name, bound, body, c) =
  match fold_opt ~sigma:(fun t -> Some t) term' with
  | Some (name', bound', body', c') ->
    let var, bindings = fresh_variable bindings name in
    let body = substitute name (id ~c ~initial:(Some name) var) body
    and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
    [ bindings |- bound =?= bound'; bindings |- body =?= body' ]
  | _ -> [ failure None ]

and congruent_pair bindings term' (lhd, rhd, _c) =
  match fold_opt ~pair:(fun t -> Some t) term' with
  | Some (lhd', rhd', _) ->
    [ bindings |- lhd =?= lhd'; bindings |- rhd =?= rhd' ]
  | _ -> [ failure None ]

and congruent_fst bindings term' (term, _c) =
  match fold_opt ~fst:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_snd bindings term' (term, _c) =
  match fold_opt ~snd:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_sum bindings term' (lhd, rhd, _c) =
  match fold_opt ~sum:(fun t -> Some t) term' with
  | Some (lhd', rhd', _) ->
    [ bindings |- lhd =?= lhd'; bindings |- rhd =?= rhd' ]
  | _ -> [ failure None ]

and congruent_inl bindings term' (term, _c) =
  match fold_opt ~inl:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_inr bindings term' (term, _c) =
  match fold_opt ~inr:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_case bindings term' (term, left, right, _c) =
  match fold_opt ~case:(fun t -> Some t) term' with
  | Some (term', left', right', _c') ->
    [
      bindings |- term =?= term'
    ; bindings |- left =?= left'
    ; bindings |- right =?= right'
    ]
  | _ -> [ failure None ]

and congruent_mu bindings term' (name, body, c) =
  match fold_opt ~mu:(fun t -> Some t) term' with
  | Some (name', body', c') ->
    let var, bindings = fresh_variable bindings name in
    let body = substitute name (id ~c ~initial:(Some name) var) body
    and body' = substitute name' (id ~c:c' ~initial:(Some name') var) body' in
    [ bindings |- body =?= body' ]
  | _ -> [ failure None ]

and congruent_fold bindings term' (term, _c) =
  match fold_opt ~fold:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_unfold bindings term' (term, _c) =
  match fold_opt ~unfold:(fun t -> Some t) term' with
  | Some (term', _) -> [ bindings |- term =?= term' ]
  | _ -> [ failure None ]

and congruent_hole bindings term' (name, reference, _c) =
  match !reference with
  | Some term -> [ bindings |- term =?= term' ]
  | None -> (
    match fold_opt ~hole:(fun t -> Some t) term' with
    | Some (name', _, _) when name = name' -> []
    | _ ->
      let () = reference := Some term' in
      [] )

and congruent_terms bindings term term' =
  let term = reduce bindings term
  and term' = reduce bindings term' in
  let term, term' =
    match fold_opt ~hole:(fun _ -> Some ()) term' with
    | Some () -> (term', term)
    | _ -> (term, term')
  in
  let proofs =
    fold ~kind:(congruent_kind term') ~int:(congruent_int term')
      ~char:(congruent_char term') ~string:(congruent_string term')
      ~id:(congruent_id term')
      ~pi:(congruent_pi bindings term')
      ~lambda:(congruent_lambda bindings term')
      ~apply:(congruent_apply bindings term')
      ~sigma:(congruent_sigma bindings term')
      ~pair:(congruent_pair bindings term')
      ~fst:(congruent_fst bindings term')
      ~snd:(congruent_snd bindings term')
      ~sum:(congruent_sum bindings term')
      ~inl:(congruent_inl bindings term')
      ~inr:(congruent_inr bindings term')
      ~case:(congruent_case bindings term')
      ~mu:(congruent_mu bindings term')
      ~fold:(congruent_fold bindings term')
      ~unfold:(congruent_unfold bindings term')
      ~hole:(congruent_hole bindings term')
      term
  in
  congruent term term' proofs

and ( =?= ) (bindings, term) term' = congruent_terms bindings term term'
