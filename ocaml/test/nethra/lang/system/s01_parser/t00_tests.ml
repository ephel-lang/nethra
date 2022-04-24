let () =
  Alcotest.(
    run "Parser Test"
      [
        T01_basic.cases
      ; T02_simple_terms.cases
      ; T03_pi_or_sigma_terms.cases
      ; T04_lambda_terms.cases
      ])
