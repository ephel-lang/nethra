let rec occurs n e =
  let open Expr in
  match e with
  | Abs (m, e) -> if m = n then 0 else occurs n e
  | App (a, e) -> occurs n e + occurs n a
  | Var m -> if n = m then 1 else 0
  | Unit | Int _ -> 0
  | Inl e | Inr e -> occurs n e

type vstack =
  | VAL
  | VAR of int * string

let rec consume i n s =
  let open Vm in
  match s with
  | [] -> failwith ("Compilation Error: " ^ n ^ " not found!")
  | VAR (c, m) :: s' when n = m ->
    if c = 1 then ([ DIG i ], s) else ([ DUP i ], VAR (c - 1, m) :: s')
  | v :: s ->
    let o, s = consume (i + 1) n s in
    (o, v :: s)

(* ------------------------------------------------------------ *)
(* Compilation layer                                            *)
(* ------------------------------------------------------------ *)

let rec compile e s =
  let open Expr in
  let open Vm in
  match e with
  | Unit -> ([ PUSH UNIT ], VAL :: s)
  | Int i -> ([ PUSH (INT i) ], VAL :: s)
  | App (l, r) ->
    let o_l, s = compile l s in
    let o_r, s = compile r s in
    (o_l @ o_r @ [ EXEC ], List.tl s)
  | Var n ->
    let o, s = consume 0 n s in
    (o, VAL :: s)
  | Abs (n, e) ->
    let c = occurs n e in
    let o, s =
      match c with
      | 0 ->
        let o, s = compile e s in
        (DROP 1 :: o, s)
      | _ -> compile e (VAR (c, n) :: s)
    in
    ([ LAMBDA o ], s)
  | Inl e ->
    let o, s = compile e s in
    (o @ [ LEFT ], List.tl s)
  | Inr e ->
    let o, s = compile e s in
    (o @ [ RIGHT ], List.tl s)

(* ------------------------------------------------------------ *)
(* Optimisation layer                                           *)
(* ------------------------------------------------------------ *)

let rec try_optimise =
  let open Vm in
  function
  | DIG 0 :: l -> l
  | LAMBDA l1 :: a :: EXEC :: l2 -> a :: (l1 @ l2)
  | PUSH _ :: DROP 1 :: l -> l
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