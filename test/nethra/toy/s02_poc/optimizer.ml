let rec optimise_sequence =
  let open Vm in
  function
  | SEQ l1 :: l2 -> l1 @ l2
  | DIG (0, _) :: l -> l
  | PUSH v :: LAMBDA l1 :: SWAP :: l2 -> LAMBDA l1 :: PUSH v :: l2
  | LAMBDA l1 :: PUSH v :: EXEC :: l2 -> PUSH v :: l1 :: l2
  | PUSH _ :: DROP (1, _) :: l -> l
  | RIGHT :: IF_LEFT (_, r) :: l' -> r :: l'
  | LEFT :: IF_LEFT (l, _) :: l' -> l :: l'
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
  let o' = optimise_instruction o in
  if o' = o then o' else optimise o'
