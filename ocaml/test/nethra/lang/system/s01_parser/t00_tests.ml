let () =
  Alcotest.(run "Parser Test" [ T01_basic.cases; T02_simple_terms.cases ])
