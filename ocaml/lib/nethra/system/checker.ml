open Nethra_ast.Term.Builders
open Nethra_ast.Term.Catamorphism
open Nethra_ast.Proof.Builders
open Nethra_ast.Bindings.Access

module Impl (Infer : Specs.Infer) = struct
  module Congruent = Congruent.Impl
  include Goal

  (*
    Γ ⊢
    -----------------------
    Γ ⊢ Type_i : Type_{i+1}
  *)
  let check_kind bindings term' (level, c) =
    let term = kind ~c (level + 1) in
    [ Congruent.(bindings |- term =?= term') ]

  (*
    l ∈ int
    -----------
    Γ ⊢ l : int
  *)
  let check_int bindings term' (_, c) =
    let term = id ~c "int" in
    [ Congruent.(bindings |- term =?= term') ]

  (*
    l ∈ char
    ------------
    Γ ⊢ l : char
  *)
  let check_char bindings term' (_, c) =
    let term = id ~c "char" in
    [ Congruent.(bindings |- term =?= term') ]

  (*
    l ∈ string
    --------------
    Γ ⊢ l : string
  *)
  let check_string bindings term' (_, c) =
    let term = id ~c "string" in
    [ Congruent.(bindings |- term =?= term') ]

  (*
    Γ ⊢
    ----------------
    Γ, x : T ⊢ x : T
  *)
  let check_id bindings term' (name, _, _) =
    match get_signature bindings name with
    | Some term -> [ Congruent.(bindings |- term =?= term') ]
    | None -> [ failure @@ Some "Unbound variable" ]

  (*
    Γ ⊢ M : S   Γ, x : M ⊢ N : T
    ----------------------------
    Γ ⊢ Π(x:M).N : T
  *)
  let rec check_pi bindings term' (name, bound, body, _implicit, _c) =
    [
      Stdlib.snd Infer.(bindings |- bound <?:> ())
    ; add_signature bindings (name, bound) |- body <?:> term'
    ]

  and check_lambda _bindings term' (_name, _body, _implicit, _c) =
    match fold_opt ~pi:(fun p -> Some p) term' with
    | Some (_name', _bound', _body', _implicit', _c') ->
      [ failure @@ Some "TODO" ]
    | None -> [ failure @@ Some "Waiting for a pi term" ]

  and check_apply bindings _term' (abstraction, _argument, _implicit, _c) =
    match Infer.(bindings |- abstraction <?:> ()) with
    | Some _t, _proof -> [ failure @@ Some "TODO" ]
    | None, _proof -> [ failure @@ Some "TODO" ]

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

  and check_type bindings term term' =
    let proofs =
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
    in
    check term term' proofs

  and ( <?:> ) (bindings, term) term' = check_type bindings term term'
end
