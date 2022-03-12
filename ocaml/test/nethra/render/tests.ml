let () =
  Alcotest.(
    run "Test"
      [
        T01_basic.cases
      ; T02_function.cases
      ; T03_pair.cases
      ; T04_sum.cases
      ; T05_mu.cases
      ])
