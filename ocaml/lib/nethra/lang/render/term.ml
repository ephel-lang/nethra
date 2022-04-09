open Format

let kind ppf (level, _) =
  if level == 0 then fprintf ppf "type" else fprintf ppf "type%d" level

let int ppf (value, _) = fprintf ppf "%d" value
let char ppf (value, _) = fprintf ppf "'%c'" value
let string ppf (value, _) = fprintf ppf "\"%s\"" value

let id ppf (value, initial, _) =
  match initial with
  | Some value -> fprintf ppf "%s" value
  | None -> fprintf ppf "%s" value

let pi ppf render (n, bound, body, implicit, _) =
  fprintf ppf
    (if implicit then "Π{%s:%a}.%a" else "Π(%s:%a).%a")
    n render bound render body

let lambda ppf render (n, body, implicit, _) =
  fprintf ppf (if implicit then "λ{%s}.(%a)" else "λ(%s).(%a)") n render body

let apply ppf render (abstraction, argument, implicit, _) =
  fprintf ppf
    (if implicit then "%a {%a}" else "%a %a")
    render abstraction render argument

let sigma ppf render (n, bound, body, _) =
  fprintf ppf "Σ(%s:%a).%a" n render bound render body

let pair ppf render (left, right, _) =
  fprintf ppf "(%a,%a)" render left render right

let fst ppf render (term, _) = fprintf ppf "fst %a" render term
let snd ppf render (term, _) = fprintf ppf "snd %a" render term

let sum ppf render (left, right, _) =
  fprintf ppf "%a | %a" render left render right

let inl ppf render (term, _) = fprintf ppf "inl %a" render term
let inr ppf render (term, _) = fprintf ppf "inr %a" render term

let case ppf render (term, left, right, _) =
  fprintf ppf "case %a %a %a" render term render left render right

let mu ppf render (n, body, _) = fprintf ppf "μ(%s).(%a)" n render body
let fold ppf render (term, _) = fprintf ppf "fold %a" render term
let unfold ppf render (term, _) = fprintf ppf "unfold %a" render term

let hole ppf render (value, reference, _) =
  match !reference with
  | Some value -> fprintf ppf "%a" render value
  | None -> fprintf ppf "%s" value

let rec render ppf t =
  Nethra_ast.Term.Destruct.fold ~kind:(kind ppf) ~int:(int ppf) ~char:(char ppf)
    ~string:(string ppf) ~id:(id ppf) ~pi:(pi ppf render)
    ~lambda:(lambda ppf render) ~apply:(apply ppf render)
    ~sigma:(sigma ppf render) ~pair:(pair ppf render) ~fst:(fst ppf render)
    ~snd:(snd ppf render) ~sum:(sum ppf render) ~inl:(inl ppf render)
    ~inr:(inr ppf render) ~case:(case ppf render) ~mu:(mu ppf render)
    ~fold:(fold ppf render) ~unfold:(unfold ppf render) ~hole:(hole ppf render)
    t
