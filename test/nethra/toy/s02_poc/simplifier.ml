let is_push =
  let open Vm in
  function PUSH _ | LAMBDA _ -> true | _ -> false

let is_replace =
  let open Vm in
  function CAR | CDR | LEFT | RIGHT -> true | _ -> false

let rec last =
  let open Preface.Option.Functor.Infix in
  function
  | [] -> None
  | [ a ] -> Some (a, [])
  | a :: l -> last l <&> fun (b, l) -> (b, a :: l)

let rec simplify_sequence =
  let open Vm in
  function
  | DIG (0, _) :: l -> l
  | DIG (1, _) :: l -> SWAP :: l
  | EXEC :: DROP (i, n) :: l when i > 0 -> DROP (i + 1, n) :: EXEC :: l
  | a :: DROP (i, n) :: l when is_push a && i > 0 -> DROP (i - 1, n) :: a :: l
  | a :: DROP (i, n) :: l when is_replace a && i > 0 -> DROP (i, n) :: a :: l
  | DUP (i, n) :: DROP (j, _) :: l when j = i + 1 -> DIG (i, n) :: l
  | DUP (i, m) :: DROP (j, n) :: l when j > i ->
    DROP (j - 1, n) :: DUP (i, m) :: l
  | DUP (i, m) :: DROP (j, n) :: l when j > 0 ->
    DROP (j - 1, n) :: DUP (i - 1, m) :: l
  | DIG (i, m) :: DROP (j, n) :: l when j > 0 ->
    if j > i
    then DROP (j - 1, n) :: DIG (i, m) :: l
    else DROP (j - 1, n) :: DIG (i - 1, m) :: l
  | DUP _ :: IF_LEFT (DROP (0, _) :: l, DROP (0, _) :: r) :: s ->
    IF_LEFT (l, r) :: s
  | a :: s -> simplify_instruction a :: simplify_sequence s
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