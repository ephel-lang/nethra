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
