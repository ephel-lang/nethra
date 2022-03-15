open Nethra_ast.Ast.Term.Builders
open Nethra_ast.Ast.Term.Catamorphism
open Nethra_ast.Ast.Bindings.Access
open Substitution
open Stdlib.Fun
open Preface.Option.Monad
open Preface.Option.Foldable

let reduce_id reduce bindings (name, _, _) =
  get_definition bindings name >>= reduce bindings

let reduce_apply reduce bindings (abstraction, argument, implicit, c) =
  reduce bindings abstraction
  >>= fold_opt ~lambda:(fun (n, body, implicit', _) ->
          if implicit = implicit'
          then return (substitute n argument body)
          else if implicit'
          then return (apply ~c ~implicit (substitute n (hole n) body) argument)
          else None )
  >>= reduce bindings

let reduce_fst reduce bindings (term, _c) =
  fold_opt ~pair:(fun (lhd, _, _) -> return lhd) term >>= reduce bindings

let reduce_snd reduce bindings (term, _c) =
  fold_opt ~pair:(fun (_, rhd, _) -> return rhd) term >>= reduce bindings

let reduce_case reduce bindings (term, left, right, _) =
  reduce bindings term
  >>= fold_opt
        ~inl:(fun (term, _) -> return (apply left term))
        ~inr:(fun (term, _) -> return (apply right term))
  >>= reduce bindings

let reduce_hole reduce bindings (_, reference, _) =
  !reference >>= reduce bindings

let rec reduce_opt bindings term =
  fold_right const
    (fold_opt
       ~id:(reduce_id reduce_opt bindings)
       ~apply:(reduce_apply reduce_opt bindings)
       ~fst:(reduce_fst reduce_opt bindings)
       ~snd:(reduce_snd reduce_opt bindings)
       ~case:(reduce_case reduce_opt bindings)
       ~hole:(reduce_hole reduce_opt bindings)
       term )
    term
  |> return

let reduce bindings term = fold_right const (reduce_opt bindings term) term
