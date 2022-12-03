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

  let refl = localize (Reserved._REFL_ <&> fun _ -> Refl)
  let var = localize (identifier <&> fun a -> Var a)
  let int = localize (integer <&> fun a -> Literal (Int a))
  let string = localize (Delimited.string <&> fun a -> Literal (String a))
  let char = localize (Delimited.char <&> fun a -> Literal (Char a))

  let rec pi_implicit =
    lazy
      ( localize
          ( Reserved._LACC_
          >~> rep identifier
          <~< Reserved._COLON_
          <~> do_lazy term
          <~< Reserved._RACC_
          <~< Reserved._ARROW_
          <~> do_lazy term )
      <&> function
      | Localized (((idl, t1), t2), r) ->
        List.fold_right
          (fun id t2 -> Localized (Pi (id, t1, t2, true), r))
          idl t2 )

  and sigma_or_pi_explicit =
    lazy
      ( localize
          ( do_try (Reserved._LPAR_ >~> rep identifier <~< Reserved._COLON_)
          <~> do_lazy term
          <~< Reserved._RPAR_
          <~> ( Reserved._ARROW_
              <&> Stdlib.Fun.const true
              <|> (Reserved._PRODUCT_ <&> Stdlib.Fun.const false) )
          <~> do_lazy term )
      <&> function
      | Localized ((((idl, t1), a), t2), r) ->
        if a
        then
          List.fold_right
            (fun id t2 -> Localized (Pi (id, t1, t2, false), r))
            idl t2
        else
          List.fold_right
            (fun id t2 -> Localized (Sigma (id, t1, t2), r))
            idl t2 )

  and sigma_or_pi = lazy (do_lazy sigma_or_pi_explicit <|> do_lazy pi_implicit)
  and block = lazy (Reserved._LPAR_ >~> do_lazy term <~< Reserved._RPAR_)

  and lambda_explicit =
    lazy
      ( localize
          ( do_try
              ( Reserved._LPAR_
              >~> rep identifier
              <~< Reserved._RPAR_
              <~< Reserved._DOT_ )
          <~> do_lazy sterm )
      <&> function
      | Localized ((idl, b), r) ->
        List.fold_right (fun id b -> Localized (Lambda (id, b, false), r)) idl b
      )

  and lambda_implicit =
    lazy
      ( localize
          ( Reserved._LACC_
          >~> rep identifier
          <~< Reserved._RACC_
          <~< Reserved._DOT_
          <~> do_lazy sterm )
      <&> function
      | Localized ((idl, b), r) ->
        List.fold_right (fun id b -> Localized (Lambda (id, b, true), r)) idl b
      )

  and lambda = lazy (do_lazy lambda_explicit <|> do_lazy lambda_implicit)

  and equal =
    lazy
      (localize
         ( Reserved._REFL_EQUALS_
         >~> do_lazy sterm
         <~> do_lazy sterm
         <&> fun (t1, t2) -> Equal (t1, t2) ) )

  and subst =
    lazy
      (localize
         ( Reserved._SUBST_
         >~> do_lazy sterm
         <~< Reserved._BY_
         <~> do_lazy sterm
         <&> fun (t1, t2) -> Subst (t1, t2) ) )

  and let_in =
    lazy
      (localize
         ( Reserved._LET_
         >~> identifier
         <~> opt (Reserved._COLON_ >~> do_lazy term)
         <~< Reserved._EQUAL_
         <~> do_lazy term
         <~< Reserved._IN_
         <~> do_lazy sterm
         <&> fun (((id, t), v), f) -> Let (id, t, v, f) ) )

  and recursive =
    lazy
      (localize
         ( Reserved._REC_
         >~> Reserved._LPAR_
         >~> identifier
         <~< Reserved._COLON_
         <~> do_lazy term
         <~< Reserved._RPAR_
         <~< Reserved._DOT_
         <~> do_lazy sterm
         <&> fun ((id, t1), t2) -> Rec (id, t1, t2) ) )

  and case =
    lazy
      (localize
         ( Reserved._CASE_
         >~> do_lazy sterm
         <~> do_lazy sterm
         <~> do_lazy sterm
         <&> fun ((t0, t1), t2) -> Case (t0, t1, t2) ) )

  and sig_record =
    lazy
      (localize
         ( do_try (Reserved._SIG_ <~> Reserved._STRUCT_)
         >~> opt_rep
               ( Reserved._SIG_
               >~> identifier
               <~< Reserved._COLON_
               <~> do_lazy term )
         <~< Reserved._END_
         <&> fun l -> Record (S_Sig, l) ) )

  and access_record =
    lazy
      (localize
         ( do_try (Reserved._FROM_ >~> identifier)
         <~> do_lazy sterm
         <&> fun (id, e) -> Access (e, id) ) )

  and val_record =
    lazy
      (localize
         ( Reserved._VAL_
         <~> Reserved._STRUCT_
         >~> opt_rep
               ( Reserved._VAL_
               >~> identifier
               <~> opt (Reserved._COLON_ >~> do_lazy term)
               <~< Reserved._EQUAL_
               <~> do_lazy term
               <&> function
               | (n, None), e -> (n, e)
               | (n, Some t), Localized (e, r) ->
                 (n, Localized (Annotation (Localized (e, r), t), r)) )
         <~< Reserved._END_
         <&> fun l -> Record (S_Val, l) ) )

  and build_in keyword operation =
    lazy
      (localize (keyword >~> do_lazy sterm <&> fun t -> BuildIn (operation, t)))

  and all_build_in =
    lazy
      ( do_lazy (build_in Reserved._FST_ Fst)
      <|> do_lazy (build_in Reserved._SND_ Snd)
      <|> do_lazy (build_in Reserved._INL_ Inl)
      <|> do_lazy (build_in Reserved._INR_ Inr)
      <|> do_lazy (build_in Reserved._FOLD_ Fold)
      <|> do_lazy (build_in Reserved._UNFOLD_ Unfold) )

  and sterm =
    lazy
      ( do_lazy access_record
      <|> kind
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
      <|> refl
      <|> do_lazy equal
      <|> do_lazy subst
      <|> do_lazy sig_record
      <|> do_lazy val_record )

  and term_and_apply =
    lazy
      ( do_lazy sterm
      <~> opt_rep
            ( Reserved._LACC_
            >~> do_lazy term
            <~< Reserved._RACC_
            <&> (fun a -> (a, true))
            <|> (do_lazy sterm <&> fun a -> (a, false)) )
      <&> fun (a, params) ->
      List.fold_left
        (fun a (p, i) ->
          Localized
            ( Apply (a, p, i)
            , Construct.create
                (Access.first (region a))
                (Access.last (region p)) ) )
        a params )

  and term_and_apply_and_disjunction =
    lazy
      (localize
         ( do_lazy term_and_apply
         <~> opt (Reserved._DISJUNCTION_ >~> do_lazy aterm)
         <&> function Localized (a, _), None -> a | a, Some b -> Sum (a, b) ) )

  and aterm =
    lazy
      (localize
         ( do_lazy term_and_apply_and_disjunction
         <~> opt
               ( Reserved._COMMA_
               <&> Stdlib.Fun.const `Pair
               <|> (Reserved._PRODUCT_ <&> Stdlib.Fun.const `Product)
               <~> do_lazy aterm )
         <&> function
         | Localized (a, _), None -> a
         | a, Some (`Pair, b) -> Pair (a, b)
         | a, Some (`Product, b) -> Sigma ("_", a, b) ) )

  and arrow_or_term =
    lazy
      (localize
         ( do_lazy aterm
         <~> opt (Reserved._ARROW_ >~> do_lazy term)
         <&> function
         | Localized (a, _), None -> a | a, Some b -> Pi ("_", a, b, false) ) )

  and term = lazy (do_lazy sigma_or_pi <|> do_lazy arrow_or_term)

  let term = do_lazy term
end
