include module type of Judgment

val ( =?= ) :
     'a Nethra_ast.Hypothesis.t * 'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Proof.t
(** The congruence provides an intuitive DSL

    ``` hypothesis |- term1 =?= term2 ``` *)
