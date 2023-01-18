let rec expand_sequence =
  let open Vm in
  function
  | IF_LEFT (l, r) :: s when List.length s > 0 -> [ IF_LEFT (l @ s, r @ s) ]
  | a :: l -> expand_instruction a :: expand_sequence l
  | [] -> []

and expand_instruction =
  let open Vm in
  function
  | LAMBDA (n, l) -> LAMBDA (n, expand_sequence l)
  | IF_LEFT (l, r) -> IF_LEFT (expand_sequence l, expand_sequence r)
  | o -> o

let rec expand o =
  let o' = expand_sequence o in
  if o' = o then o' else expand o'