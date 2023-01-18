type value =
  | Var of string
  | Dup of string
  | Val of Vm.value
  | Code of string * Vm.instruction list
  | Exec of value * value
  | Left of value
  | Right of value
  | Pair of value * value
  | Car of value
  | Cdr of value
  | IfLeft of value * Vm.instruction list * Vm.instruction list

type stack =
  | Stack of value * stack
  | Protected of value list

let rec render_value ppf =
  let open Format in
  function
  | Var n -> fprintf ppf "%s" n
  | Dup n -> fprintf ppf "copy(%s)" n
  | Val v -> Vm.render_value ppf v
  | Code (_, c) -> Vm.render ppf c
  | Exec (a, b) -> fprintf ppf "Exec(%a,%a)" render_value a render_value b
  | Car a -> fprintf ppf "Car(%a)" render_value a
  | Cdr a -> fprintf ppf "Cdr(%a)" render_value a
  | Pair (a, b) -> fprintf ppf "Pair(%a,%a)" render_value a render_value b
  | Left a -> fprintf ppf "Left(%a)" render_value a
  | Right a -> fprintf ppf "Right(%a)" render_value a
  | IfLeft (a, l, r) ->
    fprintf ppf "IfLeft(%a,%a,%a)" render_value a Vm.render l Vm.render r

let rec render_values ppf =
  let open Format in
  function
  | [] -> () | a :: s -> fprintf ppf "%a; %a" render_value a render_values s

let rec get_index i n = function
  | Var m :: _ when n = m -> i
  | _ :: s -> get_index (i + 1) n s
  | [] -> failwith ("Variable not found: " ^ n)

let generate (o, s) =
  let open Vm in
  let push_in r a = a :: r in
  let rec generate s r =
    match s with
    | [] -> r
    | [ Var _ ] -> r
    | Dup n :: s -> DUP (get_index 1 n s, n) |> push_in r |> generate s
    | Val a :: s -> PUSH a |> push_in r |> generate s
    | Code (n, a) :: s -> LAMBDA (n, a) |> push_in r |> generate s
    | Exec (a, c) :: s -> EXEC |> push_in r |> generate (c :: a :: s)
    | Car a :: s -> CAR |> push_in r |> generate (a :: s)
    | Cdr a :: s -> CDR |> push_in r |> generate (a :: s)
    | Pair (pl, pr) :: s -> PAIR |> push_in r |> generate (pl :: pr :: s)
    | Left a :: s -> LEFT |> push_in r |> generate (a :: s)
    | Right a :: s -> RIGHT |> push_in r |> generate (a :: s)
    | IfLeft (a, pl, pr) :: s ->
      IF_LEFT (pl, pr) |> push_in r |> generate (a :: s)
    | s -> failwith ("Generation error for: " ^ Render.to_string render_values s)
  in
  generate s o

let rec remove_at l i =
  if i = 0
  then (List.hd l, List.tl l)
  else
    let h, t = remove_at (List.tl l) (i - 1) in
    (h, List.hd l :: t)

let rec optimise_instruction i s =
  let open Vm in
  function
  | PUSH v -> ([], Val v :: s)
  | EXEC -> (
    match s with
    | a :: Code (_, c) :: s -> optimise (i ^ "  ") (a :: s) c
    | a :: c :: s -> ([], Exec (c, a) :: s)
    | _ -> ([ EXEC ], s) )
  | LAMBDA (n, a) ->
    let o = generate (optimise (i ^ "  ") [ Var n ] a) in
    ([], Code (n, o) :: s)
  | DIG (i, n) -> ([ DIG (i, n) ], s)
  | DUP (i, n) -> (
    match List.nth_opt s i with
    | None -> ([ DUP (i, n) ], s)
    | Some (Var f) -> ([], Dup f :: s)
    | Some a -> ([], a :: s) )
  | DROP (i, n) -> (
    if List.length s <= i
    then ([ DROP (i, n) ], s)
    else
      (* Dropped effects should be prohibited *)
      match remove_at s i with
      | Var _, _ -> ([ DROP (i, n) ], s)
      | _, s -> ([], s) )
  | SWAP -> (
    match s with a :: b :: s -> ([], b :: a :: s) | _ -> ([ SWAP ], s) )
  | LEFT -> ( match s with a :: s -> ([], Left a :: s) | [] -> ([ LEFT ], s) )
  | RIGHT -> (
    match s with a :: s -> ([], Right a :: s) | [] -> ([ RIGHT ], s) )
  | IF_LEFT (l, r) -> (
    match s with
    | Left v :: s -> optimise (i ^ "  ") (v :: s) l
    | Right v :: s -> optimise (i ^ "  ") (v :: s) r
    | _ ->
      (* Not optimal code right now! *)
      let l = generate (optimise (i ^ "  ") [] l) in
      let r = generate (optimise (i ^ "  ") [] r) in
      if List.length s = 0
      then ([ IF_LEFT (l, r) ], s)
      else ([], IfLeft (List.hd s, l, r) :: List.tl s) )
  | CAR -> (
    match s with
    | Pair (a, _) :: s -> ([], a :: s)
    | a :: s -> ([], Car a :: s)
    | [] -> ([ CAR ], s) )
  | CDR -> (
    match s with
    | Pair (_, a) :: s -> ([], a :: s)
    | a :: s -> ([], Cdr a :: s)
    | [] -> ([ CDR ], s) )
  | PAIR -> (
    match s with a :: b :: s -> ([], Pair (a, b) :: s) | _ -> ([ PAIR ], s) )

and optimise i s c =
  let _ =
    print_string
      ( i
      ^ "Optimize "
      ^ Vm.to_string c
      ^ " | "
      ^ Render.to_string render_values s
      ^ "\n" )
  in
  let c', s' =
    match c with
    | [] -> ([], s)
    | a :: l ->
      let o, s = optimise_instruction (i ^ "  ") s a in
      (* review this part *)
      if o = []
      then optimise (i ^ "  ") s l
      else (o @ generate (optimise (i ^ "  ") [] l), s)
  in
  let _ =
    print_string
      (i ^ Vm.to_string c' ^ " | " ^ Render.to_string render_values s' ^ "\n")
  in
  (c', s')

let optimise o = generate (optimise "" [] o)