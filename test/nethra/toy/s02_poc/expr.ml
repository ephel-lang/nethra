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
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Let of string * exp * exp

(*
let f x y = ...

becomes

let f p = let x = fst p in y = snd p in ...
----
f a b

becomes

f (a,b)
*)

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
  | Pair (l, r) -> fprintf ppf "(%a,%a)" render l render r
  | Fst e -> fprintf ppf "fst(%a)" render e
  | Snd e -> fprintf ppf "snd(%a)" render e
  | Let (n, e, f) -> fprintf ppf "let %s = %a in %a" n render e render f

let to_string o = Render.to_string render o
