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

let render l =
  let open Nethra_syntax_source in
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = Location.Render.render formatter l in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer

let string_of_error r =
  let open Preface_stdlib.Result.Bifunctor in
  let to_error = function
    | `AbstractionError s -> s
    | `Freevars _ -> "freevars found"
    | `SyntaxError l ->
      "syntax error at " ^ render (fst l) ^ " waiting: " ^ snd l
    | `FreeVarsError l ->
      List.fold_left
        (fun a b -> a ^ " " ^ b)
        "freevars error: " (List.map snd l)
    | `TypeError s -> s
  in
  map_snd to_error r
