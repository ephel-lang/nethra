open Common
open Nethra.Syntax.Source
open Nethra.Toy.Parser

let parser_pi_implicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term
    @@ Parsec.source (Utils.chars_of_string "{a:b} -> c")
  and expected = (Some "{a:b} -> c", true) in
  Alcotest.(check (pair (option string) bool)) "pi implicit" expected result

let parser_pi_explicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term
    @@ Parsec.source (Utils.chars_of_string "(a:b) -> c")
  and expected = (Some "(a:b) -> c", true) in
  Alcotest.(check (pair (option string) bool)) "pi explicit" expected result

let parser_sigma () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "(a:b) * c")
  and expected = (Some "(a:b) * c", true) in
  Alcotest.(check (pair (option string) bool)) "sigma" expected result

let parser_simple_sigma () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "b * c")
  and expected = (Some "(b) * (c)", true) in
  Alcotest.(check (pair (option string) bool)) "product" expected result

let parser_left () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "left a")
  and expected = (Some "left (a)", true) in
  Alcotest.(check (pair (option string) bool)) "left" expected result

let parser_right () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "right a")
  and expected = (Some "right (a)", true) in
  Alcotest.(check (pair (option string) bool)) "right" expected result

let parser_pair () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "b , c")
  and expected = (Some "(b),(c)", true) in
  Alcotest.(check (pair (option string) bool)) "pair" expected result

let parser_lambda_implicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "{a}.(c)")
  and expected = (Some "{a}.(c)", true) in
  Alcotest.(check (pair (option string) bool)) "lambda implicit" expected result

let parser_apply_implicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "a {c}")
  and expected = (Some "a {c}", true) in
  Alcotest.(check (pair (option string) bool)) "apply implicit" expected result

let parser_lambda_explicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "(a).(c)")
  and expected = (Some "(a).(c)", true) in
  Alcotest.(check (pair (option string) bool)) "lambda explicit" expected result

let parser_apply_explicit () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "a c")
  and expected = (Some "a (c)", true) in
  Alcotest.(check (pair (option string) bool)) "apply explicit" expected result

let parser_let_in () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term
    @@ Parsec.source (Utils.chars_of_string "let e = a in c")
  and expected = (Some "let e = a in (c)", true) in
  Alcotest.(check (pair (option string) bool)) "let in" expected result

let parser_rec () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "rec(e).a")
  and expected = (Some "rec(e).(a)", true) in
  Alcotest.(check (pair (option string) bool)) "rec" expected result

let parser_fold () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "fold a")
  and expected = (Some "fold (a)", true) in
  Alcotest.(check (pair (option string) bool)) "fold" expected result

let parser_unfold () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "unfold a")
  and expected = (Some "unfold (a)", true) in
  Alcotest.(check (pair (option string) bool)) "unfold" expected result

let parser_sum () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "a | b")
  and expected = (Some "(a) | (b)", true) in
  Alcotest.(check (pair (option string) bool)) "sum" expected result

let parser_case () =
  let open Expression.Impl (Parsec) in
  let result =
    response render
    @@ term
    @@ Parsec.source (Utils.chars_of_string "case a b c")
  and expected = (Some "case (a) (b) (c)", true) in
  Alcotest.(check (pair (option string) bool)) "case" expected result

let parser_inl () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "inl a")
  and expected = (Some "inl (a)", true) in
  Alcotest.(check (pair (option string) bool)) "inl" expected result

let parser_inr () =
  let open Expression.Impl (Parsec) in
  let result =
    response render @@ term @@ Parsec.source (Utils.chars_of_string "inr a")
  and expected = (Some "inr (a)", true) in
  Alcotest.(check (pair (option string) bool)) "inr" expected result

let cases =
  let open Alcotest in
  ( "Expression Parser"
  , [
      test_case "pi implicit" `Quick parser_pi_implicit
    ; test_case "pi explicit" `Quick parser_pi_explicit
    ; test_case "sigma" `Quick parser_sigma
    ; test_case "product" `Quick parser_simple_sigma
    ; test_case "pair" `Quick parser_pair
    ; test_case "left" `Quick parser_left
    ; test_case "right" `Quick parser_right
    ; test_case "lambda implicit" `Quick parser_lambda_implicit
    ; test_case "apply implicit" `Quick parser_apply_implicit
    ; test_case "lambda explicit" `Quick parser_lambda_explicit
    ; test_case "apply explicit" `Quick parser_apply_explicit
    ; test_case "let in" `Quick parser_let_in
    ; test_case "rec" `Quick parser_rec
    ; test_case "fold" `Quick parser_fold
    ; test_case "unfold" `Quick parser_unfold
    ; test_case "sum" `Quick parser_sum
    ; test_case "case" `Quick parser_case
    ; test_case "inl" `Quick parser_inl
    ; test_case "inr" `Quick parser_inr
    ] )
