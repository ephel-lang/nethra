(* *)

type implicit = bool

type lit =
  | Int of int
  | Char of char
  | String of string

type 'a t =
  | Type of int * 'a option
  | Literal of lit * 'a option
  | Id of string * string option * 'a option
  | Pi of string * 'a t * 'a t * implicit * 'a option
  | Lambda of string * 'a t * implicit * 'a option
  | Apply of 'a t * 'a t * implicit * 'a option
  | Sigma of string * 'a t * 'a t * 'a option
  | Pair of 'a t * 'a t * 'a option
  | Fst of 'a t * 'a option
  | Snd of 'a t * 'a option
  | Sum of 'a t * 'a t * 'a option
  | Inl of 'a t * 'a option
  | Inr of 'a t * 'a option
  | Case of 'a t * 'a t * 'a t * 'a option
  | Mu of string * 'a t * 'a t * 'a option
  | Fold of 'a t * 'a option
  | Unfold of 'a t * 'a option
  | Hole of string * 'a t option ref * 'a option
  | Annotation of 'a t * 'a t * 'a option

module Construct = struct
  let kind ?(c = None) i = Type (i, c)
  let int ?(c = None) v = Literal (Int v, c)
  let string ?(c = None) v = Literal (String v, c)
  let char ?(c = None) v = Literal (Char v, c)
  let id ?(c = None) ?(initial = None) name = Id (name, initial, c)

  let pi ?(c = None) ?(implicit = false) n bound body =
    Pi (n, bound, body, implicit, c)

  let arrow ?(c = None) bound body = pi ~c "_" bound body

  let lambda ?(c = None) ?(implicit = false) n body =
    Lambda (n, body, implicit, c)

  let apply ?(c = None) ?(implicit = false) abs arg =
    Apply (abs, arg, implicit, c)

  let sigma ?(c = None) n bound body = Sigma (n, bound, body, c)
  let pair ?(c = None) left right = Pair (left, right, c)
  let fst ?(c = None) term = Fst (term, c)
  let snd ?(c = None) term = Snd (term, c)
  let sum ?(c = None) left right = Sum (left, right, c)
  let inl ?(c = None) term = Inl (term, c)
  let inr ?(c = None) term = Inr (term, c)
  let case ?(c = None) term left right = Case (term, left, right, c)
  let mu ?(c = None) self kind body = Mu (self, kind, body, c)
  let fold ?(c = None) term = Fold (term, c)
  let unfold ?(c = None) term = Unfold (term, c)
  let hole ?(c = None) ?(r = ref None) n = Hole (n, r, c)
  let annotation ?(c = None) term kind = Annotation (term, kind, c)
end

module Destruct = struct
  let fold ~kind ~int ~char ~string ~id ~pi ~lambda ~apply ~sigma ~pair ~fst
      ~snd ~sum ~inl ~inr ~case ~mu ~fold ~unfold ~hole ~annotation = function
    | Type (i, c) -> kind (i, c)
    | Literal (Int i, c) -> int (i, c)
    | Literal (Char i, c) -> char (i, c)
    | Literal (String i, c) -> string (i, c)
    | Id (name, initial, c) -> id (name, initial, c)
    | Pi (n, bound, body, implicit, c) -> pi (n, bound, body, implicit, c)
    | Lambda (n, body, implicit, c) -> lambda (n, body, implicit, c)
    | Apply (abs, arg, implicit, c) -> apply (abs, arg, implicit, c)
    | Sigma (n, bound, body, c) -> sigma (n, bound, body, c)
    | Pair (first, second, c) -> pair (first, second, c)
    | Fst (term, c) -> fst (term, c)
    | Snd (term, c) -> snd (term, c)
    | Sum (lhd, rhd, c) -> sum (lhd, rhd, c)
    | Inl (term, c) -> inl (term, c)
    | Inr (term, c) -> inr (term, c)
    | Case (term, left, right, c) -> case (term, left, right, c)
    | Mu (self, kind, body, c) -> mu (self, kind, body, c)
    | Fold (term, c) -> fold (term, c)
    | Unfold (term, c) -> unfold (term, c)
    | Hole (n, body, c) -> hole (n, body, c)
    | Annotation (term, kind, c) -> annotation (term, kind, c)

  let fold_opt =
    let internal_fold = fold in
    let none _ = None in
    fun ?(kind = none) ?(int = none) ?(char = none) ?(string = none)
        ?(id = none) ?(pi = none) ?(lambda = none) ?(apply = none)
        ?(sigma = none) ?(pair = none) ?(fst = none) ?(snd = none) ?(sum = none)
        ?(inl = none) ?(inr = none) ?(case = none) ?(mu = none) ?(fold = none)
        ?(unfold = none) ?(hole = none) ?(annotation = none) term ->
      internal_fold ~kind ~int ~char ~string ~id ~pi ~lambda ~apply ~sigma ~pair
        ~fst ~snd ~sum ~inl ~inr ~case ~mu ~fold ~unfold ~hole ~annotation term
end
