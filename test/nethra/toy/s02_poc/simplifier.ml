let is_push =
  let open Vm in
  function PUSH _ | LAMBDA _ -> true | _ -> false

let rec simplify_sequence =
  let open Vm in
  function
  | DIG (0, _) :: l -> l
  | DIG (1, _) :: l -> SWAP :: l
  | PUSH a :: DROP (1, n) :: l -> DROP (0, n) :: PUSH a :: l
  | DUP (i, n) :: DROP (j, _) :: l when j = i + 1 -> DIG (i, n) :: l
  | DUP (i, m) :: DROP (j, n) :: l when j > 0 ->
    if j > i
    then DROP (j - 1, n) :: DUP (i, m) :: l
    else DROP (j - 1, n) :: DUP (i - 1, m) :: l
  | DIG (i, m) :: DROP (j, n) :: l when j > 0 ->
    if j > i
    then DROP (j - 1, n) :: DIG (i, m) :: l
    else DROP (j - 1, n) :: DIG (i - 1, m) :: l
  | IF_LEFT (l, r) :: s when List.length s > 0 -> [ IF_LEFT (l @ s, r @ s) ]
  | a :: l -> simplify_instruction a :: simplify_sequence l
  | [] -> []

and simplify_instruction =
  let open Vm in
  function
  | LAMBDA (n, l) -> LAMBDA (n, simplify_sequence l)
  | IF_LEFT (l, r) -> IF_LEFT (simplify_sequence l, simplify_sequence r)
  | o -> o

let rec simplify o =
  let o' = simplify_sequence o in
  if o' = o then o' else simplify o'