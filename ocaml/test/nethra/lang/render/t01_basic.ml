open Nethra.Ast.Term
open Common

let render_type0 () =
  let repr = render @@ Builders.kind 0 in
  Alcotest.(check string) "type0" "type" repr

let render_type2 () =
  let repr = render @@ Builders.kind 2 in
  Alcotest.(check string) "type2" "type2" repr

let render_int () =
  let repr = render @@ Builders.int 42 in
  Alcotest.(check string) "int" "42" repr

let render_char () =
  let repr = render @@ Builders.char 'e' in
  Alcotest.(check string) "char" "'e'" repr

let render_string () =
  let repr = render @@ Builders.string "hello" in
  Alcotest.(check string) "string" "\"hello\"" repr

let render_id () =
  let repr = render @@ Builders.id "hello" in
  Alcotest.(check string) "string" "hello" repr

let render_id_initial () =
  let repr = render @@ Builders.id ~initial:(Some "hello") "$42" in
  Alcotest.(check string) "string" "hello" repr

let cases =
  let open Alcotest in
  ( "Render basic terms"
  , [
      test_case "type0" `Quick render_type0
    ; test_case "type2" `Quick render_type2
    ; test_case "int" `Quick render_int
    ; test_case "char" `Quick render_char
    ; test_case "string" `Quick render_string
    ; test_case "id" `Quick render_id
    ; test_case "id with initial" `Quick render_id_initial
    ] )
