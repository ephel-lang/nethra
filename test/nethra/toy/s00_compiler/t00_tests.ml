let () =
  Alcotest.(
    run "Compiler Test" [ T01_basic.cases; T02_function.cases; T03_sum.cases ])
