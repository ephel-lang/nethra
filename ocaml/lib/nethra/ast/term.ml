(* *)

type lit =
  | Int of int
  | Char of char
  | String of string

type 'a t =
  | Type of int * 'a option
  | Literal of lit * 'a option
  | Id of string * string option * 'a option
  | Pi of string * 'a t * 'a t * bool * 'a option
  | Lambda of string * 'a t * bool * 'a option
  | Apply of 'a t * 'a t * bool * 'a option
  | Sigma of string * 'a t * 'a t * 'a option
  | Pair of 'a t * 'a t * 'a option
  | Fst of 'a t * 'a option
  | Snd of 'a t * 'a option
  | Sum of 'a t * 'a t * 'a option
  | Inl of 'a t * 'a option
  | Inr of 'a t * 'a option
  | Case of 'a t * 'a t * 'a t * 'a option
  | Mu of string * 'a t * 'a option
  | Fold of 'a t * 'a option
  | Unfold of 'a t * 'a option
  | Hole of string * 'a t option ref * 'a option

module Builders = struct
  let kind ?(c = None) i = Type (i, c)
  let int ?(c = None) v = Literal (Int v, c)
  let string ?(c = None) v = Literal (String v, c)
  let char ?(c = None) v = Literal (Char v, c)
  let id ?(c = None) ?(initial = None) name = Id (name, initial, c)

  let pi ?(c = None) ?(implicit = false) n bound body =
    Pi (n, bound, body, implicit, c)

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
  let mu ?(c = None) self body = Mu (self, body, c)
  let fold ?(c = None) term = Fold (term, c)
  let unfold ?(c = None) term = Unfold (term, c)
  let hole ?(c = None) ?(r = ref None) n = Hole (n, r, c)
end

module Catamorphism = struct
  let fold ~kind ~int ~char ~string ~id ~pi ~lambda ~apply ~sigma ~pair ~fst
      ~snd ~sum ~inl ~inr ~case ~mu ~fold ~unfold ~hole = function
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
    | Mu (self, body, c) -> mu (self, body, c)
    | Fold (term, c) -> fold (term, c)
    | Unfold (term, c) -> unfold (term, c)
    | Hole (n, body, c) -> hole (n, body, c)

  let fold_default default term =
    let internal_fold = fold in
    fun ?(kind = fun _ -> default term) ?(int = fun _ -> default term)
        ?(char = fun _ -> default term) ?(string = fun _ -> default term)
        ?(id = fun _ -> default term) ?(pi = fun _ -> default term)
        ?(lambda = fun _ -> default term) ?(apply = fun _ -> default term)
        ?(sigma = fun _ -> default term) ?(pair = fun _ -> default term)
        ?(fst = fun _ -> default term) ?(snd = fun _ -> default term)
        ?(sum = fun _ -> default term) ?(inl = fun _ -> default term)
        ?(inr = fun _ -> default term) ?(case = fun _ -> default term)
        ?(mu = fun _ -> default term) ?(fold = fun _ -> default term)
        ?(unfold = fun _ -> default term) ?(hole = fun _ -> default term) ->
      internal_fold ~kind ~int ~char ~string ~id ~pi ~lambda ~apply ~sigma ~pair
        ~fst ~snd ~sum ~inl ~inr ~case ~mu ~fold ~unfold ~hole term

  let fold_opt =
    let internal_fold = fold in
    fun ?(kind = fun _ -> None) ?(int = fun _ -> None) ?(char = fun _ -> None)
        ?(string = fun _ -> None) ?(id = fun _ -> None) ?(pi = fun _ -> None)
        ?(lambda = fun _ -> None) ?(apply = fun _ -> None)
        ?(sigma = fun _ -> None) ?(pair = fun _ -> None) ?(fst = fun _ -> None)
        ?(snd = fun _ -> None) ?(sum = fun _ -> None) ?(inl = fun _ -> None)
        ?(inr = fun _ -> None) ?(case = fun _ -> None) ?(mu = fun _ -> None)
        ?(fold = fun _ -> None) ?(unfold = fun _ -> None)
        ?(hole = fun _ -> None) term ->
      internal_fold ~kind ~int ~char ~string ~id ~pi ~lambda ~apply ~sigma ~pair
        ~fst ~snd ~sum ~inl ~inr ~case ~mu ~fold ~unfold ~hole term
end
