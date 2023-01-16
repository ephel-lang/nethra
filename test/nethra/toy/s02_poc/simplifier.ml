let is_push =
  let open Vm in
  function PUSH _ | LAMBDA _ -> true | _ -> false

let rec simplify_sequence =
  let open Vm in
  function
  | SEQ l1 :: l2 -> l1 @ l2
  | DIG (0, _) :: l -> l
  | DIG (1, _) :: l -> SWAP :: l
  | PUSH a :: DROP (1, n) :: l -> DROP (0, n) :: PUSH a :: l
  | DUP (i, n) :: DROP (j, _) :: l when j = i + 1 -> DIG (i, n) :: l
  | IF_LEFT (SEQ (a :: l1), SEQ (b :: l2)) :: l when equal a b ->
    a :: IF_LEFT (SEQ l1, SEQ l2) :: l
  | a :: l -> simplify_instruction a :: simplify_sequence l
  | [] -> []

and simplify_instruction =
  let open Vm in
  function
  | LAMBDA (n, l) -> LAMBDA (n, simplify_instruction l)
  | IF_LEFT (l, r) -> IF_LEFT (simplify_instruction l, simplify_instruction r)
  | SEQ [ a ] -> a
  | SEQ l -> SEQ (simplify_sequence l)
  | DIG (0, _) -> SEQ []
  | o -> o

let rec simplify o =
  let o' = simplify_instruction o in
  if o' = o then o' else simplify o'