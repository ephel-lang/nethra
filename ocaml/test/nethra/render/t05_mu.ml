open Nethra.Ast.Term
open Common

let render_mu () =
  let repr = render @@ Builders.(mu "x" (id "x")) in
  Alcotest.(check string) "mu" "μ(x).(x)" repr

let render_fold () =
  let repr = render @@ Builders.(fold (id "x")) in
  Alcotest.(check string) "fold" "fold x" repr

let render_unfold () =
  let repr = render @@ Builders.(unfold (id "x")) in
  Alcotest.(check string) "unfold" "unfold x" repr

let cases =
  let open Alcotest in
  ( "Render mu types"
  , [
      test_case "μ(x).(x)" `Quick render_mu
    ; test_case "fold x" `Quick render_fold
    ; test_case "unfold x" `Quick render_unfold
    ] )
