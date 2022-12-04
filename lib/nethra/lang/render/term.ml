open Format

let kind ppf (level, _) =
  if level == 0 then fprintf ppf "type" else fprintf ppf "type%d" level

let int ppf (value, _) = fprintf ppf "%d" value
let char ppf (value, _) = fprintf ppf "'%c'" value
let string ppf (value, _) = fprintf ppf "\"%s\"" value

let id ppf (value, initial, _) =
  match initial with
  | Some initial -> fprintf ppf "%s" initial
  | None -> fprintf ppf "%s" value

let pi ppf render (n, bound, body, implicit, _) =
  if n = "_"
  then fprintf ppf "(%a) -> %a" render bound render body
  else
    fprintf ppf
      (if implicit then "{%s:%a} -> %a" else "(%s:%a) -> %a")
      n render bound render body

let lambda ppf render (n, body, implicit, _) =
  fprintf ppf (if implicit then "{%s}.(%a)" else "(%s).(%a)") n render body

let apply ppf render (abstraction, argument, implicit, _) =
  fprintf ppf
    (if implicit then "%a {%a}" else "%a (%a)")
    render abstraction render argument

let let_binding ppf render (name, arg, body, _) =
  fprintf ppf "let %s = %a in %a" name render arg render body

let sigma ppf render (n, bound, body, _) =
  if n = "_"
  then fprintf ppf "(%a) * %a" render bound render body
  else fprintf ppf "(%s:%a) * %a" n render bound render body

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

let mu ppf render (n, kind, body, _) =
  fprintf ppf "rec(%s:%a).(%a)" n render kind render body

let fold ppf render (term, _) = fprintf ppf "fold %a" render term
let unfold ppf render (term, _) = fprintf ppf "unfold %a" render term

let hole ppf render (name, reference, _) =
  match !reference with
  | Some value -> fprintf ppf "%a" render value
  | None -> fprintf ppf "%s" name

let annotation ppf render (term, kind, _) =
  fprintf ppf "(%a:%a)" render term render kind

let equals ppf render (lhd, rhd, _) =
  fprintf ppf "(%a = %a)" render lhd render rhd

let refl ppf _ = fprintf ppf "refl"

let subst ppf render (lhd, rhd, _) =
  fprintf ppf "subst %a by %a" render lhd render rhd

let record_sig ppf render (l, _) =
  let () = fprintf ppf "{ " in
  let () = List.iter (fun (n, e) -> fprintf ppf "%s:(%a) " n render e) l in
  fprintf ppf "}"

let record_val ppf render (l, _) =
  let () = fprintf ppf "{ " in
  let () = List.iter (fun (n, e) -> fprintf ppf "%s=(%a) " n render e) l in
  fprintf ppf "}"

let access ppf render (t, n, _) = fprintf ppf "#%s %a" n render t

let rec render ppf t =
  Nethra_lang_ast.Term.Destruct.fold ~kind:(kind ppf) ~int:(int ppf)
    ~char:(char ppf) ~string:(string ppf) ~id:(id ppf) ~pi:(pi ppf render)
    ~lambda:(lambda ppf render) ~apply:(apply ppf render)
    ~let_binding:(let_binding ppf render) ~sigma:(sigma ppf render)
    ~pair:(pair ppf render) ~fst:(fst ppf render) ~snd:(snd ppf render)
    ~sum:(sum ppf render) ~inl:(inl ppf render) ~inr:(inr ppf render)
    ~case:(case ppf render) ~mu:(mu ppf render) ~fold:(fold ppf render)
    ~unfold:(unfold ppf render) ~hole:(hole ppf render)
    ~annotation:(annotation ppf render) ~equals:(equals ppf render)
    ~refl:(refl ppf) ~subst:(subst ppf render)
    ~record_sig:(record_sig ppf render) ~record_val:(record_val ppf render)
    ~access:(access ppf render) t