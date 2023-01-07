module Expr = struct
  type kind =
    | Native of string
    | Function of kind * kind

  type exp =
    | Unit
    | Int of int
    | Abs of string * exp
    | App of exp * exp
    | Var of string
end

module VM = struct
  type value =
    | INT of int
    | UNIT

  type order =
    | PUSH of value
    | EXEC
    | LAMBDA of order list
    | DIG of int
    | DUP of int
    | DROP of int

  let render_value ppf =
    let open Format in
    function INT i -> fprintf ppf "INT(%d)" i | UNIT -> fprintf ppf "UNIT"

  let rec render_list ppf =
    let open Format in
    function
    | [] -> ()
    | [ a ] -> render ppf a
    | a :: l -> fprintf ppf "%a; %a" render a render_list l

  and render ppf =
    let open Format in
    function
    | PUSH v -> fprintf ppf "PUSH(%a)" render_value v
    | EXEC -> fprintf ppf "EXEC"
    | LAMBDA l -> fprintf ppf "{ %a }" render_list l
    | DIG i -> fprintf ppf "DIG(%d)" i
    | DUP i -> fprintf ppf "DUG(%d)" i
    | DROP i -> fprintf ppf "DROP(%d)" i
end

let render o =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = VM.render_list formatter o in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer

(* Compilation layer *)

let rec occurs n e =
  let open Expr in
  match e with
  | Abs (m, e) -> if m = n then 0 else occurs n e
  | App (a, e) -> occurs n e + occurs n a
  | Var m -> if n = m then 1 else 0
  | Unit | Int _ -> 0

type vstack =
  | VAL
  | VAR of int * string

let rec consume i n s =
  let open VM in
  match s with
  | [] -> failwith "Compilation Error"
  | VAR (c, m) :: s' when n = m ->
    if c = 1 then ([ DIG i ], s) else ([ DUP i ], VAR (c - 1, m) :: s')
  | v :: s ->
    let o, s = consume (i + 1) n s in
    (o, v :: s)

let rec compile e s =
  let open Expr in
  let open VM in
  match e with
  | Unit -> ([ PUSH UNIT ], VAL :: s)
  | Int i -> ([ PUSH (INT i) ], VAL :: s)
  | App (l, r) ->
    let o_r, s = compile r s in
    let o_l, s' = compile l [ List.hd s ] in
    (o_l @ o_r @ [ EXEC ], List.hd s' :: List.tl s)
  | Var n ->
    let o, s = consume 0 n s in
    (o, VAL :: s)
  | Abs (n, e) ->
    let c = occurs n e in
    let o, s = compile e (VAR (c, n) :: s) in
    let o = if c = 0 then DROP 1 :: o else o in
    ([ LAMBDA o ], s)

(* Optimisation layer *)

let rec try_optimise =
  let open VM in
  function
  | DIG 0 :: l -> l
  | LAMBDA [] :: a :: EXEC :: l -> a :: l
  | LAMBDA [ DROP 1; s ] :: _ :: EXEC :: l -> s :: l
  | LAMBDA s :: l ->
    let s' = try_optimise s in
    if s' = s then LAMBDA s :: try_optimise l else LAMBDA s' :: l
  | a :: l -> a :: try_optimise l
  | [] -> []

let rec optimise o =
  let o' = try_optimise o in
  if o' = o then o' else optimise o'

let compile e =
  let o, _ = compile e [] in
  optimise o