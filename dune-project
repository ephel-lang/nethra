(lang dune 3.4)
 (name nethra)
 (version 0.1)
 (maintainers "d.plaindoux@free.fr")
 (generate_opam_files true)
 (package
  (name nethra)
  (synopsis "Core language based on dependent types")
  (description "Core language based on dependent types")
  (depends
   (alcotest :with-test)
   (preface (>= 1.0.0))))