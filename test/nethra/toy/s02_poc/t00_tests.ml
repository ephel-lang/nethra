let () =
  Alcotest.(
    run "Compilation Test" [ T01_basic.cases; T02_sum.cases; T03_pair.cases ] )
