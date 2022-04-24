open Format
open Localized
open Term

let rec render_term ppf = function
  | Type i -> fprintf ppf "type%d" i
  | Var n -> fprintf ppf "%s" n
  | Literal (Int i) -> fprintf ppf "%d" i
  | Literal (String s) -> fprintf ppf "\"%s\"" s
  | Literal (Char c) -> fprintf ppf "'%c'" c
  | Pi (id, t1, t2, true) ->
    fprintf ppf "{%s:%a} -> %a" id render_localized t1 render_localized t2
  | Pi (id, t1, t2, false) ->
    fprintf ppf "(%s:%a) -> %a" id render_localized t1 render_localized t2
  | Sigma (id, t1, t2) ->
    fprintf ppf "(%s:%a) * %a" id render_localized t1 render_localized t2
  | Lambda (id, t, true) -> fprintf ppf "{%s}.%a" id render_localized t
  | Lambda (id, t, false) -> fprintf ppf "(%s).%a" id render_localized t

and render_localized ppt (Localized (t, _)) = render_term ppt t
