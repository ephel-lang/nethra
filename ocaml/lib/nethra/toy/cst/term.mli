type literal =
  | Int of int
  | String of string
  | Char of char

type t =
  | Type of int
  | Var of string
  | Literal of literal
