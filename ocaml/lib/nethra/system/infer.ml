open Nethra_ast.Term.Builders
open Nethra_ast.Term.Catamorphism
open Nethra_ast.Proof.Builders
open Nethra_ast.Bindings.Access

module Impl (Checker : Specs.Checker) = struct
  include Goal

  let infer_kind _bindings (level, c) =
    let term = kind ~c @@ (level + 1) in
    (Some term, [])

  let infer_int _bindings (_, c) =
    let term = id ~c "int" in
    (Some term, [])

  let infer_char _bindings (_, c) =
    let term = id ~c "char" in
    (Some term, [])

  let infer_string _bindings (_, c) =
    let term = id ~c "string" in
    (Some term, [])

  let infer_id bindings (name, _, _) =
    match get_signature bindings name with
    | Some term -> (Some term, [])
    | None -> (None, [ failure @@ Some ("Unbound variable " ^ name) ])

  let rec infer_pi _bindings (_name, _bound, _body, _implicit, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_lambda _bindings (_name, _body, _implicit, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_apply _bindings (_abstraction, _argument, _implicit, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_sigma _bindings (_name, _bound, _body, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_pair _bindings (_lhd, _rhd, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_fst _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_snd _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_sum _bindings (_lhd, _rhd, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_inl _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_inr _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])

  and infer_case _bindings (_term, _left, _right, _c) =
    (None, [ failure @@ Some "TODO" ])

  and infer_mu _bindings (_name, _body, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_fold _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])
  and infer_unfold _bindings (_term, _c) = (None, [ failure @@ Some "TODO" ])

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
    (term', infer term term' proofs)

  and ( <?:> ) (bindings, term) () = infer_type bindings term
end
