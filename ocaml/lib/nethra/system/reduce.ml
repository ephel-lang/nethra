open Nethra_ast.Ast.Term.Builders
open Nethra_ast.Ast.Term.Catamorphism
open Nethra_ast.Ast.Bindings.Access
open Substitution
open Preface.Option.Monad

let reduce_id reduce bindings (name, _, _) =
  get_definition bindings name >>= reduce bindings

let reduce_apply reduce bindings (abstraction, argument, implicit, c) =
  reduce bindings abstraction
  >>= fold_opt ~lambda:(fun (n, body, implicit', _) ->
          if implicit = implicit'
          then reduce bindings (substitute n argument body)
          else if implicit'
          then
            reduce bindings
              (apply ~c ~implicit (substitute n (hole n) body) argument)
          else None )

let reduce_fst reduce bindings (term, _c) =
  fold_opt ~pair:(fun (lhd, _, _) -> reduce bindings lhd) term

let reduce_snd reduce bindings (term, _c) =
  fold_opt ~pair:(fun (_, rhd, _) -> reduce bindings rhd) term

let reduce_case reduce bindings (term, left, right, _) =
  reduce bindings term
  >>= fold_opt
        ~inl:(fun (term, _) -> reduce bindings (apply left term))
        ~inr:(fun (term, _) -> reduce bindings (apply right term))

let reduce_hole reduce bindings (_, reference, _) =
  !reference >>= reduce bindings

let rec reduce_opt bindings term =
  Option.fold ~none:(Some term)
    ~some:(fun a -> Some a)
    (fold_opt
       ~id:(reduce_id reduce_opt bindings)
       ~apply:(reduce_apply reduce_opt bindings)
       ~fst:(reduce_fst reduce_opt bindings)
       ~snd:(reduce_snd reduce_opt bindings)
       ~case:(reduce_case reduce_opt bindings)
       ~hole:(reduce_hole reduce_opt bindings)
       term )

let reduce bindings term =
  Option.fold ~none:term ~some:(fun a -> a) (reduce_opt bindings term)
