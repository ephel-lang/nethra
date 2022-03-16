let () =
  Alcotest.(
    run "Test"
      [
        T01_substitution.cases
      ; T02_reduction.cases
      ; T03_congruence.cases
      ; T04_checker_basic.cases
      ; T05_checker_function.cases
      ])
