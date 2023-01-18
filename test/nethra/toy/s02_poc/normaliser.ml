let rec last =
  let open Preface.Option.Functor.Infix in
  function
  | [] -> None
  | [ a ] -> Some (a, [])
  | a :: l -> last l <&> fun (b, l) -> (b, a :: l)

let rec normalise_sequence =
  let open Vm in
  function
  | IF_LEFT (l, r) :: s -> (
    match (last l, last r) with
    | Some (a, l), Some (a', r) when a = a' -> IF_LEFT (l, r) :: a :: s
    | _, _ -> normalise_instruction (IF_LEFT (l, r)) :: normalise_sequence s )
  | a :: s -> normalise_instruction a :: normalise_sequence s
  | [] -> []

and normalise_instruction =
  let open Vm in
  function
  | LAMBDA (n, l) -> LAMBDA (n, normalise_sequence l)
  | IF_LEFT (l, r) -> IF_LEFT (normalise_sequence l, normalise_sequence r)
  | o -> o

let rec normalise o =
  let o' = normalise_sequence o in
  if o' = o then o' else normalise o'