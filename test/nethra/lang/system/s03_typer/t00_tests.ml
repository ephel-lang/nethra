let () =
  Alcotest.(
    run "Type checker Test"
      [
        T02_equivalence.cases
      ; T03_checker_basic.cases
      ; T04_checker_function.cases
      ; T05_checker_pair.cases
      ; T06_checker_sum.cases
      ; T07_checker_mu.cases
      ; T08_checker_hole.cases
      ; T09_infer_basic.cases
      ; T10_infer_function.cases
      ; T11_infer_pair.cases
      ; T12_infer_sum.cases
      ; T13_infer_mu.cases
      ] )
