type literal =
  | Int of int
  | String of string
  | Char of char

type t =
  | Type of int
  | Var of string
  | Literal of literal
  | Pi of (string * t Localized.t * t Localized.t * bool)
  | Sigma of (string * t Localized.t * t Localized.t)
  | Lambda of (string * t Localized.t * bool)
