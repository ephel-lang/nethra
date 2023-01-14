let is_push =
  let open Vm in
  function PUSH _ | LAMBDA _ -> true | _ -> false

let rec simplify_sequence =
  let open Vm in
  function
  | SEQ l1 :: l2 -> l1 @ l2
  | DIG (0, _) :: l -> l
  | DIG (1, _) :: l -> SWAP :: l
  | a :: DROP (i, n) :: l when i > 0 -> DROP (i - 1, n) :: a :: l
  | DUP (0, _)
    :: IF_LEFT (SEQ (DROP (0, _) :: l1), SEQ (DROP (0, _) :: l2))
    :: l ->
    IF_LEFT (SEQ l1, SEQ l2) :: l (* TO BE DONE IN THE OPTIMISATION *)
  | a :: l -> simplify_instruction a :: simplify_sequence l
  | [] -> []

and simplify_instruction =
  let open Vm in
  function
  | LAMBDA l -> LAMBDA (simplify_instruction l)
  | IF_LEFT (l, r) -> IF_LEFT (simplify_instruction l, simplify_instruction r)
  | SEQ [ a ] -> a
  | SEQ l -> SEQ (simplify_sequence l)
  | DIG (0, _) -> SEQ []
  | o -> o

let rec simplify o =
  let o' = simplify_instruction o in
  if o' = o then o' else simplify o'