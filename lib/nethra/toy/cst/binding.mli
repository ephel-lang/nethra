type t =
  | Signature of string * Term.t Localized.t
  | Definition of string * Term.t Localized.t option * Term.t Localized.t
