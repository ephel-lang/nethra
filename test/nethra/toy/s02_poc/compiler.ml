type vstack =
  | VAL of string
  | VAR of string

let rec render_stack ppf =
  let open Format in
  function
  | [] -> fprintf ppf ""
  | VAL s :: l -> fprintf ppf "VAL(%s), %a" s render_stack l
  | VAR n :: l -> fprintf ppf "VAR(%s), %a" n render_stack l

let consume n s =
  let rec consume i s =
    let open Vm in
    match s with
    | [] -> failwith ("Compilation Error: " ^ n ^ " not found!")
    | VAR m :: s' when n = m -> ([ DUP (i, n) ], VAR m :: s')
    | v :: s ->
      let o, s = consume (i + 1) s in
      (o, v :: s)
  in
  consume 0 s

let garbage n s =
  let rec garbage i s =
    let open Vm in
    match s with
    | [] -> failwith ("Compilation Error: " ^ n ^ " not found!")
    | VAR m :: s when n = m -> (DROP (i, n), s)
    | v :: s ->
      let o, s = garbage (i + 1) s in
      (o, v :: s)
  in
  garbage 0 s

let rec compile_binding i n e s =
  let _ =
    print_string
      ( i
      ^ "Compile Abstraction "
      ^ n
      ^ " -> "
      ^ Expr.to_string e
      ^ " | "
      ^ Render.to_string render_stack s
      ^ "\n" )
  in
  let o, s' = compile i e (VAR n :: s) in
  let g_o, s' = garbage n s' in
  let o, s' = (o @ [ g_o ], s') in
  let _ =
    print_string
      ( i
      ^ " "
      ^ Vm.to_string o
      ^ " | "
      ^ Render.to_string render_stack s'
      ^ "\n" )
  in
  (o, s')

and compile i e s =
  let open Expr in
  let open Vm in
  let _ =
    print_string
      ( i
      ^ "Compile "
      ^ Expr.to_string e
      ^ " | "
      ^ Render.to_string render_stack s
      ^ "\n" )
  in
  let o, s' =
    match e with
    (* Atoms *)
    | Unit -> ([ PUSH UNIT ], VAL "unit" :: s)
    | Int i -> ([ PUSH (INT i) ], VAL "int" :: s)
    | Var n ->
      let o, s = consume n s in
      (o, VAL n :: s)
    (* Sum *)
    | Inl e ->
      let o, s = compile (i ^ "  ") e s in
      (o @ [ LEFT ], VAL "left" :: s)
    | Inr e ->
      let o, s = compile (i ^ "  ") e s in
      (o @ [ RIGHT ], VAL "right" :: s)
    | Case (e, Abs (n, l), Abs (m, r)) ->
      let e_o, s = compile (i ^ "  ") e s in
      let l_o, _ = compile_binding (i ^ "  ") n l (List.tl s) in
      let r_o, _ = compile_binding (i ^ "  ") m r (List.tl s) in
      (e_o @ [ IF_LEFT (l_o, r_o) ], VAL "case" :: List.tl s)
    (* Product *)
    | Pair (l, r) ->
      let r_o, s = compile (i ^ "  ") r s in
      let l_o, s = compile (i ^ "  ") l (List.tl s) in
      (r_o @ l_o @ [ PAIR ], VAL "pair" :: List.tl s)
    | Fst o ->
      let l_o, s = compile (i ^ "  ") o s in
      (l_o @ [ CAR ], VAL "fst" :: List.tl s)
    | Snd o ->
      let l_o, s = compile (i ^ "  ") o s in
      (l_o @ [ CDR ], VAL "snd" :: List.tl s)
    (* Abstraction and Application *)
    | Abs (n, e) ->
      let o, _ = compile_binding (i ^ "  ") n e [] in
      ([ LAMBDA (n, o) ], VAL "lambda" :: s)
    | Let (n, e, f) ->
      let e_o, s = compile (i ^ "  ") e s in
      let l_o, s' = compile_binding (i ^ "  ") n f (List.tl s) in
      (e_o @ l_o, s')
    | App (l, r) ->
      let o_l, s = compile (i ^ "  ") l s in
      let o_r, s = compile (i ^ "  ") r s in
      (o_l @ o_r @ [ EXEC ], VAL "app" :: List.tl (List.tl s))
    | _ -> failwith ("Cannot compile expression: " ^ Expr.to_string e)
  in
  let _ =
    print_string
      (i ^ Vm.to_string o ^ " | " ^ Render.to_string render_stack s' ^ "\n")
  in
  (o, s')

let compile e = fst (compile "" e [])