include module type of Judgment

val ( =?= ) :
     'a Nethra_ast.Context.Hypothesis.t * 'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Proof.t
(** The Equivalence provides an intuitive DSL

    ``` hypothesis |- term1 =?= term2 ``` *)
