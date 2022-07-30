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
  let to_error = function
    | `AbstractionError s -> s
    | `Freevars _ -> "freevars found"
    | `SyntaxError _ -> "syntax error"
    | `FreeVarsError _ -> "freevars error"
    | `TypeError s -> s
  in
  map_snd to_error r
