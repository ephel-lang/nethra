module Impl : sig
  include module type of Goal

  val ( =?= ) :
       'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Bindings.t
    -> 'a Nethra_ast.Proof.t
  (** The congruence provides an intuitive DSL

      ``` bindings |- (term1 =?= term2) ``` *)
end
