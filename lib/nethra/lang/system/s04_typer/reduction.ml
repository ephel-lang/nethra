open Nethra_lang_ast.Term.Construct
open Nethra_lang_ast.Term.Destruct
open Nethra_lang_ast.Hypothesis.Access
open Nethra_lang_basic.Substitution
open Stdlib.Fun
open Preface.Option.Monad
open Preface.Option.Foldable

let reduce_id reduce hypothesis (name, _, _) =
  get_definition hypothesis name >>= reduce hypothesis

let reduce_apply reduce hypothesis (abstraction, argument, implicit, c) =
  reduce hypothesis abstraction
  >>= fold_opt ~lambda:(fun (n, body, implicit', _) ->
          if implicit = implicit'
          then return (substitute n argument body)
          else if implicit'
          then
            let var, _ = fresh_variable hypothesis n in
            return (apply ~c ~implicit (substitute n (hole var) body) argument)
          else None )
  >>= reduce hypothesis

let reduce_fst reduce hypothesis (term, _c) =
  reduce hypothesis term
  >>= fold_opt ~pair:(fun (lhd, _, _) -> return lhd)
  >>= reduce hypothesis

let reduce_snd reduce hypothesis (term, _c) =
  reduce hypothesis term
  >>= fold_opt ~pair:(fun (_, rhd, _) -> return rhd)
  >>= reduce hypothesis

let reduce_case reduce hypothesis (term, left, right, _) =
  reduce hypothesis term
  >>= fold_opt
        ~inl:(fun (term, _) -> return (apply left term))
        ~inr:(fun (term, _) -> return (apply right term))
  >>= reduce hypothesis

let rec reduce_opt hypothesis term =
  fold_right const
    (fold_opt
       ~id:(reduce_id reduce_opt hypothesis)
       ~apply:(reduce_apply reduce_opt hypothesis)
       ~fst:(reduce_fst reduce_opt hypothesis)
       ~snd:(reduce_snd reduce_opt hypothesis)
       ~case:(reduce_case reduce_opt hypothesis)
       term )
    term
  |> return

let reduce hypothesis term = fold_right const (reduce_opt hypothesis term) term