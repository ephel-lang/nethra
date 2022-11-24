open Nethra_lang_ast.Term

let free_vars_kind (_level, _c) = []
let free_vars_int (_value, _c) = []
let free_vars_char (_value, _c) = []
let free_vars_string (_value, _c) = []

let free_vars_id variables (n, _initial, c) =
  if List.mem n variables then [] else [ (c, n) ]

let free_vars_pi free_vars variables (n, bound, body, _implicit, _c) =
  free_vars variables bound @ free_vars (n :: variables) body

let free_vars_lambda free_vars variables (n, body, _implicit, _c) =
  free_vars (n :: variables) body

let free_vars_apply free_vars variables (abstraction, parameter, _implicit, _c)
    =
  free_vars variables abstraction @ free_vars variables parameter

let free_vars_sigma free_vars variables (n, bound, body, _c) =
  free_vars variables bound @ free_vars (n :: variables) body

let free_vars_pair free_vars variables (lhd, rhd, _c) =
  free_vars variables lhd @ free_vars variables rhd

let free_vars_fst free_vars variables (term, _c) = free_vars variables term
let free_vars_snd free_vars variables (term, _c) = free_vars variables term

let free_vars_sum free_vars variables (lhd, rhd, _c) =
  free_vars variables lhd @ free_vars variables rhd

let free_vars_inl free_vars variables (term, _c) = free_vars variables term
let free_vars_inr free_vars variables (term, _c) = free_vars variables term

let free_vars_case free_vars variables (term, left, right, _c) =
  free_vars variables term
  @ free_vars variables left
  @ free_vars variables right

let free_vars_mu free_vars variables (n, kind, body, _c) =
  free_vars variables kind @ free_vars (n :: variables) body

let free_vars_fold free_vars variables (term, _c) = free_vars variables term
let free_vars_unfold free_vars variables (term, _c) = free_vars variables term
let free_vars_hole (_v, _r, _c) = []

let free_vars_annotation free_vars variables (term, kind, _c) =
  free_vars variables term @ free_vars variables kind

let free_vars_equals free_vars variables (lhd, rhd, _c) =
  free_vars variables lhd @ free_vars variables rhd

let free_vars_refl _ = []

let free_vars_subst free_vars variables (lhd, rhd, _c) =
  free_vars variables lhd @ free_vars variables rhd

let free_vars_record free_vars variables (l, _c) =
  snd
    (List.fold_left
       (fun (variables, l) (n, t) -> (n :: variables, l @ free_vars variables t))
       (variables, []) l )

let free_vars_access free_vars variables (t, _n, _c) = free_vars variables t

let rec free_vars variables term =
  Destruct.fold ~kind:free_vars_kind ~int:free_vars_int ~char:free_vars_char
    ~string:free_vars_string ~id:(free_vars_id variables)
    ~pi:(free_vars_pi free_vars variables)
    ~lambda:(free_vars_lambda free_vars variables)
    ~apply:(free_vars_apply free_vars variables)
    ~sigma:(free_vars_sigma free_vars variables)
    ~pair:(free_vars_pair free_vars variables)
    ~fst:(free_vars_fst free_vars variables)
    ~snd:(free_vars_snd free_vars variables)
    ~sum:(free_vars_sum free_vars variables)
    ~inl:(free_vars_inl free_vars variables)
    ~inr:(free_vars_inr free_vars variables)
    ~case:(free_vars_case free_vars variables)
    ~mu:(free_vars_mu free_vars variables)
    ~fold:(free_vars_fold free_vars variables)
    ~unfold:(free_vars_unfold free_vars variables)
    ~hole:free_vars_hole
    ~annotation:(free_vars_annotation free_vars variables)
    ~equals:(free_vars_equals free_vars variables)
    ~refl:free_vars_refl
    ~subst:(free_vars_subst free_vars variables)
    ~record_sig:(free_vars_record free_vars variables)
    ~record_val:(free_vars_record free_vars variables)
    ~access:(free_vars_access free_vars variables)
    term