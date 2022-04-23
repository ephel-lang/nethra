open Format
open Localized
open Term

let render_term ppf = function
  | Type i -> fprintf ppf "type%d" i
  | Var n -> fprintf ppf "%s" n
  | Literal (Int i) -> fprintf ppf "%d" i
  | Literal (String s) -> fprintf ppf "\"%s\"" s
  | Literal (Char c) -> fprintf ppf "'%c'" c

let render_localized ppt (Localized (t, _)) = render_term ppt t
