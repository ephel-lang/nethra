type value =
  | Val of Vm.value
  | Code of string * Vm.instruction
  | Pair of value * value
  | Left of value
  | Right of value
  | Deferred of Vm.instruction

let rec render_value ppf =
  let open Format in
  function
  | Val v -> Vm.render_value ppf v
  | Code (_, c) -> Vm.render ppf c
  | Pair (a, b) -> fprintf ppf "Pair(%a,%a)" render_value a render_value b
  | Left a -> fprintf ppf "Left(%a)" render_value a
  | Right a -> fprintf ppf "Right(%a)" render_value a
  | Deferred c -> fprintf ppf "Lazy(%a)" Vm.render c

let rec render_values ppf =
  let open Format in
  function
  | [] -> () | a :: s -> fprintf ppf "%a; %a" render_value a render_values s

let seq =
  let open Vm in
  function [ a ] -> a | l -> SEQ l

let generate (o, s) =
  let open Vm in
  let rec generate = function
    | [] -> []
    | Val a :: l -> generate l @ [ PUSH a ]
    | Code (n, a) :: l -> generate l @ [ LAMBDA (n, a) ]
    | Pair (f, s) :: l ->
      generate l @ generate [ s ] @ generate [ f ] @ [ PAIR ]
    | Left a :: l -> generate l @ generate [ a ] @ [ LEFT ]
    | Right a :: l -> generate l @ generate [ a ] @ [ RIGHT ]
    | Deferred a :: l -> generate l @ [ a ]
  in
  generate s @ o

let rec remove_at l i =
  if i = 0
  then (List.hd l, List.tl l)
  else
    let h, t = remove_at (List.tl l) (i - 1) in
    (h, List.hd l :: t)

let rec optimise i s c =
  let open Vm in
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
    | SEQ [] -> ([], s)
    | SEQ (a :: l) ->
      let o, s = optimise (i ^ "  ") s a in
      if o = []
      then optimise (i ^ "  ") s (seq l)
      else (o @ generate (optimise (i ^ "  ") [] (seq l)), s)
    | PUSH v -> ([], Val v :: s)
    | EXEC -> (
      match s with
      | a :: Code (_, c) :: s -> optimise (i ^ "  ") (a :: s) c
      | _ -> ([ EXEC ], s) )
    | LAMBDA (n, a) ->
      let o = generate (optimise (i ^ "  ") [] a) in
      ([], Code (n, seq o) :: s)
    | DIG (i, n) ->
      if List.length s <= i
      then ([ DIG (i, n) ], s)
      else
        let v, s = remove_at s i in
        ([], v :: s)
    | DUP (i, n) ->
      if List.length s <= i then ([ DUP (i, n) ], s) else ([], List.nth s i :: s)
    | DROP (i, n) ->
      if List.length s <= i
      then ([ DROP (i, n) ], s)
      else
        let _, s = remove_at s i in
        ([], s)
    | SWAP -> (
      match s with a :: b :: s -> ([], b :: a :: s) | _ -> ([ SWAP ], s) )
    | LEFT -> (
      match s with a :: s -> ([], Left a :: s) | [] -> ([ LEFT ], s) )
    | RIGHT -> (
      match s with a :: s -> ([], Right a :: s) | [] -> ([ RIGHT ], s) )
    | IF_LEFT (l, r) -> (
      match s with
      | Left v :: s -> optimise (i ^ "  ") (v :: s) l
      | Right v :: s -> optimise (i ^ "  ") (v :: s) r
      | s ->
        let l = generate (optimise (i ^ "  ") [] l) in
        let r = generate (optimise (i ^ "  ") [] r) in
        ([], Deferred (IF_LEFT (seq l, seq r)) :: s) )
    | PAIR -> (
      match s with a :: b :: s -> ([], Pair (a, b) :: s) | _ -> ([ PAIR ], s) )
    | CAR -> (
      match s with Pair (a, _) :: s -> ([], a :: s) | _ -> ([ CAR ], s) )
    | CDR -> (
      match s with Pair (_, a) :: s -> ([], a :: s) | _ -> ([ CDR ], s) )
  in
  let _ =
    print_string
      ( i
      ^ Vm.to_string (seq c')
      ^ " | "
      ^ Render.to_string render_values s'
      ^ "\n" )
  in
  (c', s')

let optimise o = seq (generate (optimise "" [] o))