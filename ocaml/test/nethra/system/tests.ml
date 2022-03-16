let () =
  Alcotest.(
    run "Test"
      [
        T01_substitution.cases
      ; T02_reduce.cases
      ; T03_checker_basic.cases
      ; T04_checker_function.cases
      ])
