open Nethra.Ast
open Nethra.System
module TypeChecker = Checker.Checker (Infer.Infer)

let check_type0_type1 () =
  let bindings = Bindings.Builders.create
  and term = Term.Builders.kind 0
  and term' = Term.Builders.kind 1 in
  let proof = TypeChecker.(bindings |- (term <?:> term')) in
  Alcotest.(check bool) "|- type0:type1" (Proof.is_success proof) true

let cases =
  let open Alcotest in
  ("Check basic terms", [ test_case "|- type0:type1" `Quick check_type0_type1 ])
