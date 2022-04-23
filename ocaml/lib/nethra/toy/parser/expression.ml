open Nethra_toy_cst.Term
open Nethra_toy_cst.Localized
open Nethra_syntax_parser.Parsers

module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) =
struct
  open Atomic (Parsec)
  open Eval (Parsec)
  open Operator (Parsec)
  open Literal (Parsec)
  open Occurrence (Parsec)
  open Monad (Parsec)
  open Basic.Impl (Parsec)

  let kind =
    localize (Reserved._TYPE_ >~> (integer <|> return 0))
    <&> fun (a, r) -> Localized (Type a, r)

  let var = localize identifier <&> fun (a, r) -> Localized (Var a, r)
  let int = localize integer <&> fun (a, r) -> Localized (Literal (Int a), r)

  let string =
    localize Delimited.string <&> fun (a, r) -> Localized (Literal (String a), r)

  let char =
    localize Delimited.char <&> fun (a, r) -> Localized (Literal (Char a), r)
end
