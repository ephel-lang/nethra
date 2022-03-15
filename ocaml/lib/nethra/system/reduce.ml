open Nethra_ast.Ast.Term.Builders
open Nethra_ast.Ast.Term.Catamorphism
open Nethra_ast.Ast.Bindings.Access
open Substitution

let reduce_id reduce bindings (name, _, _) =
  Option.bind (get_definition bindings name) (reduce bindings)

let reduce_apply reduce bindings (abstraction, argument, implicit, c) =
  match reduce bindings abstraction with
  | None -> None
  | Some term ->
    fold_opt
      ~lambda:(fun (n, body, implicit', _) ->
        if implicit = implicit'
        then reduce bindings (substitute n argument body)
        else if implicit'
        then
          reduce bindings
            (apply ~c ~implicit (substitute n (hole n) body) argument)
        else None )
      term

let reduce_fst reduce bindings (term, _c) =
  fold_opt ~pair:(fun (lhd, _, _) -> reduce bindings lhd) term

let reduce_snd reduce bindings (term, _c) =
  fold_opt ~pair:(fun (_, rhd, _) -> reduce bindings rhd) term

let reduce_case reduce bindings (term, left, right, _) =
  match reduce bindings term with
  | None -> None
  | Some term ->
    fold_opt
      ~inl:(fun (term, _) -> reduce bindings (apply left term))
      ~inr:(fun (term, _) -> reduce bindings (apply right term))
      term

let reduce_hole reduce bindings (_, reference, _) =
  match !reference with None -> None | Some e -> reduce bindings e

let rec reduce_opt bindings term =
  match
    fold_opt
      ~id:(reduce_id reduce_opt bindings)
      ~apply:(reduce_apply reduce_opt bindings)
      ~fst:(reduce_fst reduce_opt bindings)
      ~snd:(reduce_snd reduce_opt bindings)
      ~case:(reduce_case reduce_opt bindings)
      ~hole:(reduce_hole reduce_opt bindings)
      term
  with
  | None -> Some term
  | some -> some

let reduce bindings term =
  match reduce_opt bindings term with None -> term | Some term -> term
