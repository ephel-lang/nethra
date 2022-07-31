open Nethra_lang_ast.Term

let freevars_kind (_level, _c) = []
let freevars_int (_value, _c) = []
let freevars_char (_value, _c) = []
let freevars_string (_value, _c) = []

let freevars_id variables (n, _initial, c) =
  if List.mem n variables then [] else [ (c, n) ]

let freevars_pi freevars variables (n, bound, body, _implicit, _c) =
  freevars variables bound @ freevars (n :: variables) body

let freevars_lambda freevars variables (n, body, _implicit, _c) =
  freevars (n :: variables) body

let freevars_apply freevars variables (abstraction, parameter, _implicit, _c) =
  freevars variables abstraction @ freevars variables parameter

let freevars_sigma freevars variables (n, bound, body, _c) =
  freevars variables bound @ freevars (n :: variables) body

let freevars_pair freevars variables (lhd, rhd, _c) =
  freevars variables lhd @ freevars variables rhd

let freevars_fst freevars variables (term, _c) = freevars variables term
let freevars_snd freevars variables (term, _c) = freevars variables term

let freevars_sum freevars variables (lhd, rhd, _c) =
  freevars variables lhd @ freevars variables rhd

let freevars_inl freevars variables (term, _c) = freevars variables term
let freevars_inr freevars variables (term, _c) = freevars variables term

let freevars_case freevars variables (term, left, right, _c) =
  freevars variables term @ freevars variables left @ freevars variables right

let freevars_mu freevars variables (n, kind, body, _c) =
  freevars variables kind @ freevars (n :: variables) body

let freevars_fold freevars variables (term, _c) = freevars variables term
let freevars_unfold freevars variables (term, _c) = freevars variables term
let freevars_hole (_v, _r, _c) = []

let freevars_annotation freevars variables (term, kind, _c) =
  freevars variables term @ freevars variables kind

let freevars_equals freevars variables (lhd, rhd, _c) =
  freevars variables lhd @ freevars variables rhd

let freevars_refl _ = []

let rec freevars variables term =
  Destruct.fold ~kind:freevars_kind ~int:freevars_int ~char:freevars_char
    ~string:freevars_string ~id:(freevars_id variables)
    ~pi:(freevars_pi freevars variables)
    ~lambda:(freevars_lambda freevars variables)
    ~apply:(freevars_apply freevars variables)
    ~sigma:(freevars_sigma freevars variables)
    ~pair:(freevars_pair freevars variables)
    ~fst:(freevars_fst freevars variables)
    ~snd:(freevars_snd freevars variables)
    ~sum:(freevars_sum freevars variables)
    ~inl:(freevars_inl freevars variables)
    ~inr:(freevars_inr freevars variables)
    ~case:(freevars_case freevars variables)
    ~mu:(freevars_mu freevars variables)
    ~fold:(freevars_fold freevars variables)
    ~unfold:(freevars_unfold freevars variables)
    ~hole:freevars_hole
    ~annotation:(freevars_annotation freevars variables)
    ~equals:(freevars_equals freevars variables)
    ~refl:freevars_refl term
