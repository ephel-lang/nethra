open Nethra.Lang.Ast.Proof
open Nethra.Lang.Render.Proof

let rec check = function
  | [] -> true
  | (_, None) :: _ -> false
  | (_, Some proof) :: l ->
    if is_success proof
    then check l
    else
      let _ = render Format.std_formatter proof in
      check l && false

let string_of_error r =
  let open Preface_stdlib.Result.Bifunctor in
  let open Stdlib.Fun in
  map_snd (const "Error") r

(*
  | Result.Ok ok -> Result.Ok ok
  | Result.Error (`AbstractionError s) -> Result.Error s
  | Result.Error (`Freevars _) -> Result.Error "freevars found"
  | Result.Error (`SyntaxError _) -> Result.Error "syntax error"
  | Result.Error (`TypeError s) -> Result.Error s
*)