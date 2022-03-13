module Infer : sig
  include module type of Goal

  val ( <?:> ) :
       'a Nethra_ast.Term.t
    -> unit
    -> 'a Nethra_ast.Bindings.t
    -> 'a Nethra_ast.Term.t option * 'a Nethra_ast.Proof.t
end
