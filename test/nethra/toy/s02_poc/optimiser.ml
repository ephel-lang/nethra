type value =
  | Val of Vm.value
  | Code of Vm.instruction
  | Pair of value * value
  | Left of value
  | Right of value
  | Deferred of Vm.instruction

let generate (o, s) =
  let open Vm in
  let rec generate = function
    | [] -> []
    | Val a :: l -> generate l @ [ PUSH a ]
    | Code a :: l -> generate l @ [ LAMBDA a ]
    | Pair (f, s) :: l ->
      generate l @ generate [ s ] @ generate [ f ] @ [ PAIR ]
    | Left a :: l -> generate l @ generate [ a ] @ [ LEFT ]
    | Right a :: l -> generate l @ generate [ a ] @ [ RIGHT ]
    | Deferred a :: l -> generate l @ [ a ]
  in
  match generate s @ o with [ a ] -> a | l -> SEQ l

let rec remove_at l i =
  if i = 0
  then (List.hd l, List.tl l)
  else
    let h, t = remove_at (List.tl l) (i - 1) in
    (h, List.hd l :: t)

let rec optimise s =
  let open Vm in
  function
  | SEQ [] -> ([], s)
  | SEQ (a :: l) ->
    let o, s = optimise s a in
    if o = []
    then optimise s (SEQ l)
    else (o @ [ generate (optimise [] (SEQ l)) ], s)
  | PUSH v -> ([], Val v :: s)
  | EXEC -> (
    match s with
    | a :: Code c :: s -> optimise (a :: s) c
    | _ -> ([], Deferred EXEC :: s) )
  | LAMBDA a ->
    let o = generate (optimise [] a) in
    ([], Code o :: s)
  | DIG (i, n) ->
    if List.length s <= i
    then ([], Deferred (DIG (i, n)) :: s)
    else
      let v, s = remove_at s i in
      ([], v :: s)
  | DUP (i, n) ->
    if List.length s <= i
    then ([], Deferred (DUP (i, n)) :: s)
    else ([], List.nth s i :: s)
  | DROP (i, n) ->
    if List.length s <= i
    then ([], Deferred (DROP (i, n)) :: s)
    else
      let _, s = remove_at s i in
      ([], s)
  | SWAP -> (
    match s with
    | a :: b :: s -> ([], b :: a :: s)
    | _ -> ([], Deferred SWAP :: s) )
  | LEFT ->
    if List.length s < 1
    then ([], Deferred LEFT :: s)
    else ([], Left (List.hd s) :: List.tl s)
  | RIGHT ->
    if List.length s < 1
    then ([], Deferred RIGHT :: s)
    else ([], Right (List.hd s) :: List.tl s)
  | IF_LEFT (l, r) -> (
    match s with
    | Left v :: s -> optimise (v :: s) l
    | Right v :: s -> optimise (v :: s) r
    | s -> ([], Deferred (IF_LEFT (l, r)) :: s) )
  | PAIR -> (
    match s with
    | a :: b :: s -> ([], Pair (a, b) :: s)
    | _ -> ([], Deferred PAIR :: s) )
  | CAR -> (
    match s with
    | Pair (a, _) :: s -> ([], a :: s)
    | _ -> ([], Deferred CAR :: s) )
  | CDR -> (
    match s with
    | Pair (_, a) :: s -> ([], a :: s)
    | _ -> ([], Deferred CDR :: s) )

let optimise o = generate (optimise [] o)
