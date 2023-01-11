type vstack =
  | VAL
  | VAR of int * string

let rec render_stack ppf =
  let open Format in
  function
  | [] -> fprintf ppf ""
  | VAL :: l -> fprintf ppf "VAL, %a" render_stack l
  | VAR (i, n) :: l -> fprintf ppf "VAR(%d,%s), %a" i n render_stack l

let rec occurrences n e =
  let open Expr in
  match e with
  | Abs (m, e) -> if m = n then 0 else occurrences n e
  | App (a, e) -> occurrences n e + occurrences n a
  | Var m -> if n = m then 1 else 0
  | Unit | Int _ -> 0
  | Inl e | Inr e -> occurrences n e
  | Case (e, l, r) -> occurrences n e + max (occurrences n l) (occurrences n r)

let check_occurrences e s =
  let open Vm in
  let rec check_occurrences i e s =
    let open Vm in
    match s with
    | [] -> ([], [])
    | VAL :: s ->
      let o, s = check_occurrences (i + 1) e s in
      (o, VAL :: s)
    | VAR (_, n) :: s -> (
      let o, s = check_occurrences (i + 1) e s in
      match occurrences n e with
      | 0 -> (o @ [ DROP (i, n) ], s)
      | c -> (o, VAR (c, n) :: s) )
  in
  let o, s = check_occurrences 0 e s in
  (SEQ o, s)

let consume n s =
  let rec consume i s =
    let open Vm in
    match s with
    | [] -> failwith ("Compilation Error: " ^ n ^ " not found!")
    | VAR (c, m) :: s' when n = m ->
      if c = 1
      then ([ DIG (i, n) ], VAL :: s')
      else ([ DUP (i, n) ], VAR (c - 1, m) :: s')
    | v :: s ->
      let o, s = consume (i + 1) s in
      (o, v :: s)
  in
  consume 0 s

let rec compile_abstraction n e s =
  let open Vm in
  let c = occurrences n e in
  match c with
  | 0 ->
    let o, s = compile e s in
    (SEQ [ DROP (0, n); o ], s)
  | _ -> compile e (VAR (c, n) :: s)

and compile e s =
  let _ =
    print_string ("Compile " ^ Expr.to_string e ^ " | " ^ Render.to_string render_stack s ^ "\n")
  in
  let open Expr in
  let open Vm in
  match e with
  | Unit -> (PUSH UNIT, VAL :: s)
  | Int i -> (PUSH (INT i), VAL :: s)
  | App (l, r) ->
    let o_r, s = compile r s in
    let o_l, s' = compile l [ List.hd s ] in
    (SEQ [ o_l; o_r; EXEC ], List.hd s' :: List.tl s)
  | Var n ->
    let o, s = consume n s in
    (SEQ o, VAL :: s)
  | Abs (n, e) ->
    let o, s = compile_abstraction n e s in
    (LAMBDA o, VAL :: s)
  | Inl e ->
    let o, s = compile e s in
    (SEQ [ o; LEFT ], VAL :: s)
  | Inr e ->
    let o, s = compile e s in
    (SEQ [ o; RIGHT ], VAL :: s)
  | Case (e, Abs (n, l), Abs (m, r)) ->
    let e_o, s = compile e s in
    let l_o, s' = compile_abstraction n l (List.tl s) in
    let l_d, _ = check_occurrences l s' in
    let r_o, s' = compile_abstraction m r (List.tl s) in
    let r_d, _ = check_occurrences r s' in
    (SEQ [ e_o; IF_LEFT (SEQ [ l_o; l_d ], SEQ [ r_o; r_d ]) ], VAL :: s)
  | _ -> failwith ("Cannot compile expression: " ^ Expr.to_string e)

let compile e = fst (compile e [])
