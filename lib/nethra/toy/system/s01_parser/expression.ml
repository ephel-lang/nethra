open Nethra_toy_cst.Term
open Nethra_toy_cst.Localized
open Nethra_syntax_parser.Parsers
open Nethra_syntax_source.Region
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
    localize (Reserved._TYPE_ >~> (integer <|> return 0) <&> fun a -> Type a)

  let var = localize (identifier <&> fun a -> Var a)
  let int = localize (integer <&> fun a -> Literal (Int a))
  let string = localize (Delimited.string <&> fun a -> Literal (String a))
  let char = localize (Delimited.char <&> fun a -> Literal (Char a))

  let rec pi_implicit () =
    localize
      ( Reserved._LACC_
      >~> identifier
      <~< Reserved._COLON_
      <~> do_lazy term
      <~< Reserved._RACC_
      <~< Reserved._ARROW_
      <~> do_lazy term
      <&> fun ((id, t1), t2) -> Pi (id, t1, t2, true) )

  and sigma_or_pi_explicit () =
    localize
      ( do_try (Reserved._LPAR_ >~> identifier <~< Reserved._COLON_)
      <~> do_lazy term
      <~< Reserved._RPAR_
      <~> ( Reserved._ARROW_
          <&> Stdlib.Fun.const true
          <|> (Reserved._PRODUCT_ <&> Stdlib.Fun.const false) )
      <~> do_lazy term
      <&> fun (((id, t1), a), t2) ->
      if a then Pi (id, t1, t2, false) else Sigma (id, t1, t2) )

  and sigma_or_pi () = do_lazy sigma_or_pi_explicit <|> do_lazy pi_implicit
  and block () = Reserved._LPAR_ >~> do_lazy term <~< Reserved._RPAR_

  and lambda_explicit () =
    localize
      ( do_try
          (Reserved._LPAR_ >~> identifier <~< Reserved._RPAR_ <~< Reserved._DOT_)
      <~> do_lazy sterm
      <&> fun (id, b) -> Lambda (id, b, false) )

  and lambda_implicit () =
    localize
      ( Reserved._LACC_
      >~> identifier
      <~< Reserved._RACC_
      <~< Reserved._DOT_
      <~> do_lazy sterm
      <&> fun (id, b) -> Lambda (id, b, true) )

  and lambda () = do_lazy lambda_explicit <|> do_lazy lambda_implicit

  and let_in () =
    localize
      ( Reserved._LET_
      >~> identifier
      <~< Reserved._EQUAL_
      <~> do_lazy term
      <~< Reserved._IN_
      <~> do_lazy sterm
      <&> fun ((id, t1), t2) -> Let (id, t1, t2) )

  and recursive () =
    localize
      ( Reserved._REC_
      >~> Reserved._LPAR_
      >~> identifier
      <~< Reserved._RPAR_
      <~< Reserved._DOT_
      <~> do_lazy sterm
      <&> fun (id, t1) -> Rec (id, t1) )

  and case () =
    localize
      ( Reserved._CASE_
      >~> do_lazy sterm
      <~> do_lazy sterm
      <~> do_lazy sterm
      <&> fun ((t0, t1), t2) -> Case (t0, t1, t2) )

  and build_in keyword operation () =
    localize (keyword >~> do_lazy sterm <&> fun t -> BuildIn (operation, t))

  and all_build_in () =
    do_lazy (build_in Reserved._FST_ Fst)
    <|> do_lazy (build_in Reserved._SND_ Snd)
    <|> do_lazy (build_in Reserved._INL_ Inl)
    <|> do_lazy (build_in Reserved._INR_ Inr)
    <|> do_lazy (build_in Reserved._FOLD_ Fold)
    <|> do_lazy (build_in Reserved._UNFOLD_ Unfold)

  and sterm () =
    kind
    <|> var
    <|> int
    <|> string
    <|> char
    <|> do_lazy lambda
    <|> do_lazy let_in
    <|> do_lazy recursive
    <|> do_lazy case
    <|> do_lazy all_build_in
    <|> do_lazy block

  and term_and_apply () =
    do_lazy sterm
    <~> opt_rep
          ( Reserved._LACC_
          >~> do_lazy sterm
          <~< Reserved._RACC_
          <&> (fun a -> (a, true))
          <|> (do_lazy sterm <&> fun a -> (a, false)) )
    <&> fun (a, params) ->
    List.fold_left
      (fun a (p, i) ->
        Localized
          ( Apply (a, p, i)
          , Construct.create
              ~first:(Access.first (region a))
              ~last:(Access.last (region p)) ) )
      a params

  and term_and_apply_and_disjunction () =
    localize
      ( do_lazy term_and_apply
      <~> opt (Reserved._DISJUNCTION_ >~> do_lazy aterm)
      <&> function Localized (a, _), None -> a | a, Some b -> Sum (a, b) )

  and term_and_apply_and_disjunction_and_pair () =
    localize
      ( do_lazy term_and_apply_and_disjunction
      <~> opt
            ( Reserved._COMMA_
            <&> Stdlib.Fun.const `Pair
            <|> (Reserved._PRODUCT_ <&> Stdlib.Fun.const `Product)
            <~> do_lazy aterm )
      <&> function
      | Localized (a, _), None -> a
      | a, Some (`Pair, b) -> Pair (a, b)
      | a, Some (`Product, b) -> Sigma ("_", a, b) )

  and aterm () = do_lazy term_and_apply_and_disjunction_and_pair

  and arrow_or_term () =
    localize
      ( do_lazy aterm
      <~> opt (Reserved._ARROW_ >~> do_lazy term)
      <&> function
      | Localized (a, _), None -> a | a, Some b -> Pi ("_", a, b, false) )

  and term () = do_lazy sigma_or_pi <|> do_lazy arrow_or_term

  let term = term ()
end
