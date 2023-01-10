type kind =
  | Native of string
  | Function of kind * kind

type exp =
  | Unit
  | Int of int
  | Abs of string * exp
  | App of exp * exp
  | Var of string
  | Inl of exp
  | Inr of exp
  | Case of exp * exp * exp

(* Renderer *)

let rec render ppf =
  let open Format in
  function
  | Abs (n, c) -> fprintf ppf "fun %s -> %a" n render c
  | App (l, r) -> fprintf ppf "%a (%a)" render l render r
  | Var n -> fprintf ppf "%s" n
  | Unit -> fprintf ppf "unit"
  | Int i -> fprintf ppf "%d" i
  | Inl c -> fprintf ppf "(inl %a)" render c
  | Inr c -> fprintf ppf "(inr %a)" render c
  | Case (c, l, r) -> fprintf ppf "case %a (%a) (%a)" render c render l render r

let to_string o =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = render formatter o in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer
