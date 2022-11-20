let () =
  Alcotest.(
    run "Compiler Test"
      [
        T01_basic.cases
      ; T02_function.cases
      ; T03_product.cases
      ; T04_sum.cases
      ; T05_equal.cases
      ; T06_record.cases
      ])