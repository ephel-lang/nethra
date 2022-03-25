module type Theory = sig
  val type_in_type : bool
end

module type Equivalence = sig
  include module type of Judgment

  val ( =?= ) :
       'a Nethra_ast.Context.Hypothesis.t * 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Proof.t
  (** The Equivalence provides an intuitive DSL

      ``` hypothesis |- term1 =?= term2 ``` *)
end

module type Checker = sig
  include module type of Judgment

  val ( <?:> ) :
       'a Nethra_ast.Context.Hypothesis.t * 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Proof.t
  (** The type checker provides an intuitive DSL

      ``` hypothesis |- term <?:> type ``` *)
end

module type Infer = sig
  include module type of Judgment

  val ( <:?> ) :
       'a Nethra_ast.Context.Hypothesis.t * 'a Nethra_ast.Term.t
    -> unit
    -> 'a Nethra_ast.Term.t option * 'a Nethra_ast.Proof.t
  (** The type inference provides an intuitive DSL

      ``` hypothesis |- term <:?> () ``` *)
end
