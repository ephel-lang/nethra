open Nethra_ast.Term
open Nethra_ast.Term.Builders

let rec substitute name value term =
  let subs_kind (level, c) = kind ~c level
  and subs_int (value, c) = int ~c value
  and subs_char (value, c) = char ~c value
  and subs_string (value, c) = string ~c value
  and subs_id (n, initial, c) = if n = name then value else id ~c ~initial n
  and subs_pi (n, bound, body, implicit, c) =
    pi ~c ~implicit n
      (substitute name value bound)
      (if n = name then body else substitute name value body)
  and subs_lambda (n, body, implicit, c) =
    lambda ~c ~implicit n (if n = name then body else substitute name value body)
  and subs_apply (abstraction, parameter, implicit, c) =
    apply ~c ~implicit
      (substitute name value abstraction)
      (substitute name value parameter)
  and subs_sigma (n, bound, body, c) =
    sigma ~c n
      (substitute name value bound)
      (if n = name then body else substitute name value body)
  and subs_pair (lhd, rhd, c) =
    pair ~c (substitute name value lhd) (substitute name value rhd)
  and subs_fst (term, c) = fst ~c (substitute name value term)
  and subs_snd (term, c) = snd ~c (substitute name value term)
  and subs_sum (lhd, rhd, c) =
    sum ~c (substitute name value lhd) (substitute name value rhd)
  and subs_inl (term, c) = inl ~c (substitute name value term)
  and subs_inr (term, c) = inr ~c (substitute name value term)
  and subs_case (term, left, right, c) =
    case ~c
      (substitute name value term)
      (substitute name value left)
      (substitute name value right)
  and subs_mu (n, body, c) =
    mu ~c n (if n = name then body else substitute name value body)
  and subs_fold (term, c) = fold ~c (substitute name value term)
  and subs_unfold (term, c) = unfold ~c (substitute name value term)
  and subs_hole (v, r, c) = hole ~c ~r v in
  Catamorphism.fold ~kind:subs_kind ~int:subs_int ~char:subs_char
    ~string:subs_string ~id:subs_id ~pi:subs_pi ~lambda:subs_lambda
    ~apply:subs_apply ~sigma:subs_sigma ~pair:subs_pair ~fst:subs_fst
    ~snd:subs_snd ~sum:subs_sum ~inl:subs_inl ~inr:subs_inr ~case:subs_case
    ~mu:subs_mu ~fold:subs_fold ~unfold:subs_unfold ~hole:subs_hole term
