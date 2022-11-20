open Nethra_syntax_source
open Nethra_syntax_parser.Parsers
open Nethra_toy_cst.Localized

module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) =
struct
  open Atomic (Parsec)
  open Eval (Parsec)
  open Operator (Parsec)
  open Literal (Parsec)
  open Occurrence (Parsec)
  open Monad (Parsec)

  let comment_line =
    string "--"
    >~> opt_rep (not (char '\n'))
    <~< opt (char '\n')
    <&> Utils.string_of_chars

  let comment_block =
    string "-{"
    >~> opt_rep (not (string "}-"))
    <~< string "}-"
    <&> Utils.string_of_chars

  let spaces = rep (char_in_string " \b\t\n\r") <&> Utils.string_of_chars
  let skip = opt_rep (comment_line <|> comment_block <|> spaces)
  let token p = p <~< skip
  let localize p = token (locate p <&> fun (a, r) -> Localized (a, r))

  let operators =
    [ "->"; "."; "("; ")"; "{"; "}"; ":"; "*"; "|"; "="; "--"; "—{"; "}-"; "@" ]

  let keywords =
    [
      "sig"
    ; "val"
    ; "type"
    ; "case"
    ; "inl"
    ; "inr"
    ; "fst"
    ; "snd"
    ; "rec"
    ; "fold"
    ; "unfold"
    ; "let"
    ; "in"
    ; "refl"
    ; "equals"
    ; "subst"
    ; "by"
    ; "struct"
    ; "end"
    ; "from"
    ]

  let alpha = char_in_range ('A', 'Z') <|> char_in_range ('a', 'z') <|> char '_'
  let special = char_in_string "@&#-=/:?*$^<>()§![]{}"
  let digit = char_in_range ('0', '9')

  let identifier =
    token
      ( alpha
      <~> opt_rep (char '_' <|> alpha <|> digit)
      <&> (fun (e, l) -> e :: l)
      <&> Utils.string_of_chars
      <?> fun s -> Stdlib.not (List.mem s keywords) )

  let operator =
    token
      ( special
      <~> opt_rep (char '_' <|> special)
      <&> (fun (e, l) -> e :: l)
      <&> Utils.string_of_chars
      <?> fun s -> Stdlib.not (List.mem s operators) )

  module Reserved = struct
    let string s = string s <|> fail ~message:(Some ("Waiting for '" ^ s ^ "'"))
    let _ARROW_ = token (string "->")
    let _DOT_ = token (string ".")
    let _LPAR_ = token (string "(")
    let _RPAR_ = token (string ")")
    let _LACC_ = token (string "{")
    let _RACC_ = token (string "}")
    let _COLON_ = token (string ":")
    let _PRODUCT_ = token (string "*")
    let _COMMA_ = token (string ",")
    let _DISJUNCTION_ = token (string "|")
    let _EQUAL_ = token (string "=")
    let _SIG_ = token (string "sig")
    let _VAL_ = token (string "val")
    let _TYPE_ = token (string "type")
    let _CASE_ = token (string "case")
    let _INL_ = token (string "inl")
    let _INR_ = token (string "inr")
    let _FST_ = token (string "fst")
    let _SND_ = token (string "snd")
    let _REC_ = token (string "rec")
    let _FOLD_ = token (string "fold")
    let _UNFOLD_ = token (string "unfold")
    let _LET_ = token (string "let")
    let _IN_ = token (string "in")
    let _REFL_ = token (string "refl")
    let _REFL_EQUALS_ = token (string "equals")
    let _SUBST_ = token (string "subst")
    let _BY_ = token (string "by")
    let _STRUCT_ = token (string "struct")
    let _END_ = token (string "end")
    let _FROM_ = token (string "from")
  end
end