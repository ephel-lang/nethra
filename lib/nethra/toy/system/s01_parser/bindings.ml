open Nethra_toy_cst.Binding
open Nethra_syntax_parser.Parsers
open Nethra_syntax_parser.Specs

module Impl (Parsec : PARSEC with type Source.e = char) = struct
  open Atomic (Parsec)
  open Eval (Parsec)
  open Operator (Parsec)
  open Literal (Parsec)
  open Occurrence (Parsec)
  open Monad (Parsec)
  open Basic.Impl (Parsec)
  open Expression.Impl (Parsec)

  let signature =
    Reserved._SIG_
    >~> identifier
    <~< Reserved._COLON_
    <~> term
    <&> fun (i, t) -> Signature (i, t)

  let definition =
    Reserved._VAL_
    >~> identifier
    <~< Reserved._EQUAL_
    <~> term
    <&> fun (i, t) -> Definition (i, t)

  let binding = signature <|> definition
  let bindings = skip >~> opt_rep binding <~< eos
end
