type value =
  | INT of int
  | UNIT

type order =
  | PUSH of value
  | EXEC
  | LAMBDA of order list
  | DIG of int
  | DUP of int
  | DROP of int
  | LEFT
  | RIGHT

let render_value ppf =
  let open Format in
  function INT i -> fprintf ppf "INT(%d)" i | UNIT -> fprintf ppf "UNIT"

let rec render_list ppf =
  let open Format in
  function
  | [] -> ()
  | [ a ] -> render ppf a
  | a :: l -> fprintf ppf "%a; %a" render a render_list l

and render ppf =
  let open Format in
  function
  | PUSH v -> fprintf ppf "PUSH(%a)" render_value v
  | EXEC -> fprintf ppf "EXEC"
  | LAMBDA l -> fprintf ppf "LAMBDA(%a)" render_list l
  | DIG i -> fprintf ppf "DIG(%d)" i
  | DUP i -> fprintf ppf "DUP(%d)" i
  | DROP i -> fprintf ppf "DROP(%d)" i
  | LEFT -> fprintf ppf "LEFT"
  | RIGHT -> fprintf ppf "RIGHT"

let to_string o =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = render_list formatter o in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer