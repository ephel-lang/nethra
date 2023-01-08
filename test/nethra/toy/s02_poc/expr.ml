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