open Nethra_ast.Term.Builders
open Nethra_ast.Term.Catamorphism
open Nethra_ast.Proof.Builders
open Nethra_ast.Bindings.Access

module Checker (Infer : module type of Infer.Infer) = struct
  include Common

  let check_kind bindings term' (level, c) =
    let term = kind ~c (level + 1) in
    Congruent.(bindings |- (term =?= term'))

  let check_int bindings term' (_, c) =
    let term = id ~c "int" in
    Congruent.(bindings |- (term =?= term'))

  let check_char bindings term' (_, c) =
    let term = id ~c "char" in
    Congruent.(bindings |- (term =?= term'))

  let check_string bindings term' (_, c) =
    let term = id ~c "string" in
    Congruent.(bindings |- (term =?= term'))

  let check_id bindings term' (name, _, _) =
    match get_signature bindings name with
    | Some term -> Congruent.(bindings |- (term =?= term'))
    | None -> failure @@ Some "Unbound variable"

  let rec check_pi bindings term' (name, bound, body, _, c) =
    let bindings = add_signature bindings (name, bound) in
    bindings |- (body <?:> term')

  and check_lambda bindings term' (name, body, implicit, c) =
    match fold_opt ~pi:(fun p -> Some p) term' with
    | Some (name', bound', body', implicit', c') -> failure @@ Some "TODO"
    | None -> failure @@ Some "Waiting for a pi term"

  and check_apply bindings term' (abstraction, argument, implicit, c) =
    match Infer.(bindings |- (abstraction <?:> ())) with
    | Some t, proof -> failure @@ Some "TODO"
    | None, proof -> failure @@ Some "TODO"

  and check_sigma bindings term' (name, bound, body, c) =
    let bindings = add_signature bindings (name, bound) in
    bindings |- (body <?:> term')

  and check_pair bindings term' (lhd, rhd, c) =
    failure @@ Some "TODO"

  and check_fst bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_snd bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_sum bindings term' (lhd, rhd, c) =
    failure @@ Some "TODO"

  and check_inl bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_inr bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_case bindings term' (term, left, right, c) =
    failure @@ Some "TODO"

  and check_mu bindings term' (name, body, c) =
    failure @@ Some "TODO"

  and check_fold bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_unfold bindings term' (term, c) =
    failure @@ Some "TODO"

  and check_hole bindings term' (name, value, c) =
    failure @@ Some "TODO"

  and check bindings term term' =
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
      ~fst:(check_fst bindings term')
      ~snd:(check_snd bindings term')
      ~sum:(check_sum bindings term')
      ~inl:(check_inl bindings term')
      ~inr:(check_inr bindings term')
      ~case:(check_case bindings term')
      ~mu:(check_mu bindings term')
      ~fold:(check_fold bindings term')
      ~unfold:(check_unfold bindings term')
      ~hole:(check_hole bindings term')
      term

  and ( <?:> ) term term' bindings = check bindings term term'
end
