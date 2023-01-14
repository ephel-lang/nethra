let is_push =
  let open Vm in
  function PUSH _ | LAMBDA _ -> true | _ -> false

let rec optimise_sequence =
  let open Vm in
  function
  | SEQ l1 :: l2 -> l1 @ l2
  | DIG (0, _) :: l -> l
  | DIG (1, _) :: l -> SWAP :: l
  | DUP (0, _) :: DROP (1, _) :: l -> l
  | a :: DROP (i, n) :: l when is_push a && i > 0 -> DROP (i - 1, n) :: a :: l
  | a :: DROP (0, _) :: l when is_push a -> l
  | a :: b :: SWAP :: l when is_push a && is_push b -> b :: a :: l
  | LAMBDA l1 :: PUSH v :: EXEC :: l2 -> PUSH v :: l1 :: l2
  | DUP (0, _)
    :: IF_LEFT (SEQ (DROP (0, _) :: l1), SEQ (DROP (0, _) :: l2))
    :: l ->
    IF_LEFT (SEQ l1, SEQ l2) :: l
  | RIGHT :: IF_LEFT (_, r) :: l' -> r :: l'
  | LEFT :: IF_LEFT (l, _) :: l' -> l :: l'
  | a :: b :: PAIR :: CAR :: l when is_push a && is_push b -> a :: l
  | a :: b :: PAIR :: CDR :: l when is_push a && is_push b -> b :: l
  | a :: l -> optimise_instruction a :: optimise_sequence l
  | [] -> []

and optimise_instruction =
  let open Vm in
  function
  | LAMBDA l -> LAMBDA (optimise_instruction l)
  | IF_LEFT (l, r) -> IF_LEFT (optimise_instruction l, optimise_instruction r)
  | SEQ [ a ] -> a
  | SEQ l -> SEQ (optimise_sequence l)
  | DIG (0, _) -> SEQ []
  | o -> o

let rec optimise o =
  let open Vm in
  let _ = print_string ("Optimise " ^ to_string o ^ "\n") in
  let o' = optimise_instruction o in
  if o' = o then o' else optimise o'