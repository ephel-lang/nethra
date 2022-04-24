open Nethra_toy_cst.Term
open Nethra_toy_cst.Localized
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

  let kind =
    localize (Reserved._TYPE_ >~> (integer <|> return 0))
    <&> fun (a, r) -> Localized (Type a, r)

  let var = localize identifier <&> fun (a, r) -> Localized (Var a, r)
  let int = localize integer <&> fun (a, r) -> Localized (Literal (Int a), r)

  let string =
    localize Delimited.string <&> fun (a, r) -> Localized (Literal (String a), r)

  let char =
    localize Delimited.char <&> fun (a, r) -> Localized (Literal (Char a), r)

  let rec pi_impl () =
    localize
      ( Reserved._LACC_
      >~> identifier
      <~< Reserved._COLON_
      <~> do_lazy term
      <~< Reserved._RACC_
      <~< Reserved._ARROW_
      <~> do_lazy term )
    <&> fun (((id, t1), t2), r) -> Localized (Pi (id, t1, t2, true), r)

  and sigma_or_pi_expl () =
    localize
      ( do_try (Reserved._LPAR_ >~> identifier <~< Reserved._COLON_)
      <~> do_lazy term
      <~< Reserved._RPAR_
      <~> ( Reserved._ARROW_
          <&> Stdlib.Fun.const true
          <|> (Reserved._PRODUCT_ <&> Stdlib.Fun.const false) )
      <~> do_lazy term )
    <&> fun ((((id, t1), a), t2), r) ->
    if a
    then Localized (Pi (id, t1, t2, false), r)
    else Localized (Sigma (id, t1, t2), r)

  and sigma_or_pi () = do_lazy sigma_or_pi_expl <|> do_lazy pi_impl
  and block () = Reserved._LPAR_ >~> do_lazy term <~< Reserved._RPAR_

  and lambda_expl () =
    localize
      ( do_try
          (Reserved._LPAR_ >~> identifier <~< Reserved._RPAR_ <~< Reserved._DOT_)
      <~> do_lazy sterm )
    <&> fun ((id, b), r) -> Localized (Lambda (id, b, false), r)

  and lambda_impl () =
    localize
      ( Reserved._LACC_
      >~> identifier
      <~< Reserved._RACC_
      <~< Reserved._DOT_
      <~> do_lazy sterm )
    <&> fun ((id, b), r) -> Localized (Lambda (id, b, true), r)

  and lambda () = do_lazy lambda_expl <|> do_lazy lambda_impl

  and sterm () =
    kind
    <|> var
    <|> int
    <|> string
    <|> char
    <|> do_lazy lambda
    <|> do_lazy block

  and term () = do_lazy sigma_or_pi <|> sterm ()
end
