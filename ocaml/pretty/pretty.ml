let kind ppf (level, _) = Format.fprintf ppf "type%d" level
let int ppf (value, _) = Format.fprintf ppf "%d" value
let char ppf (value, _) = Format.fprintf ppf "'%c'" value
let string ppf (value, _) = Format.fprintf ppf "\"%s\"" value

let id ppf (value, initial, _) =
  match initial with
  | Some value -> Format.fprintf ppf "%s" value
  | None -> Format.fprintf ppf "%s" value

let pi ppf render (n, bound, body, implicit, _) =
  if implicit
  then Format.fprintf ppf "Π{%s:%a}.%a" n render bound render body
  else Format.fprintf ppf "Π(%s:%a).%a" n render bound render body

let lambda ppf render (n, body, implicit, _) =
  if implicit
  then Format.fprintf ppf "λ({%s).%a" n render body
  else Format.fprintf ppf "λ((%s).%a" n render body

let apply ppf render (abstraction, argument, implicit, _) =
  if implicit
  then Format.fprintf ppf "%a (%a)" render abstraction render argument
  else Format.fprintf ppf "%a {%a}" render abstraction render argument

let sigma ppf render (n, bound, body, _) =
  Format.fprintf ppf "Σ(%s:%a).%a" n render bound render body

let pair ppf render (left, right, _) =
  Format.fprintf ppf "(%a,%a)" render left render right

let fst ppf render (term, _) = Format.fprintf ppf "fst %a" render term
let snd ppf render (term, _) = Format.fprintf ppf "snd %a" render term

let sum ppf render (left, right, _) =
  Format.fprintf ppf "%a | %a" render left render right

let inl ppf render (term, _) = Format.fprintf ppf "inl %a" render term
let inr ppf render (term, _) = Format.fprintf ppf "inr %a" render term


let case ppf render (term, left, right, _) =
  Format.fprintf ppf "case %a %a %a" render term render left render right

let mu ppf render (n, body, _) = Format.fprintf ppf "μ((%s).%a" n render body
let fold ppf render (term, _) = Format.fprintf ppf "fold %a" render term
let unfold ppf render (term, _) = Format.fprintf ppf "unfold %a" render term

let hole ppf render (value, reference, _) =
  match !reference with
  | Some value -> Format.fprintf ppf "%a" render value
  | None -> Format.fprintf ppf "%s" value

let rec render ppf t =
  Nethra_ast.Term.Catamorphism.fold ~kind:(kind ppf) ~int:(int ppf)
    ~char:(char ppf) ~string:(string ppf) ~id:(id ppf) ~pi:(pi ppf render)
    ~lambda:(lambda ppf render) ~apply:(apply ppf render)
    ~sigma:(sigma ppf render) ~pair:(pair ppf render) ~fst:(fst ppf render)
    ~snd:(snd ppf render) ~sum:(sum ppf render) ~inl:(inl ppf render)
    ~inr:(inr ppf render) ~case:(case ppf render) ~mu:(mu ppf render)
    ~fold:(fold ppf render) ~unfold:(unfold ppf render) ~hole:(hole ppf render)
    t
