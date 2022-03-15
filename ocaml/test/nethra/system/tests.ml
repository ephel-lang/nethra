let () =
  Alcotest.(
    run "Test"
      [ T01_substitution.cases; T01_reduce.cases; T01_checker_basic.cases ])
