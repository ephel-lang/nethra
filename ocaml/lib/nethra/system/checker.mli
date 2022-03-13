module Checker : functor (Infer : module type of Infer.Infer) -> sig
  include module type of Common

  val ( <?:> ) :
       'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Bindings.t
    -> 'a Nethra_ast.Proof.t
end
