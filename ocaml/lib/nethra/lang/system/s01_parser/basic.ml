open Nethra_syntax_source
open Nethra_syntax_parser.Parsers

module L0 (Source : Specs.SOURCE with type e = char) = struct
  module Parsec = Parsec (Source)
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
  let localize p = locate (token p)
end
