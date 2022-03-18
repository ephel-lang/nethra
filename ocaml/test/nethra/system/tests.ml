let () =
  Alcotest.(
    run "Test"
      [
        T01_substitution.cases
      ; T02_reduction.cases
      ; T03_congruence.cases
      ; T04_checker_basic.cases
      ; T05_checker_function.cases
      ; T06_checker_pair.cases
      ; T07_checker_sum.cases
      ; T08_checker_mu.cases
      ])
