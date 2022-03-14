module Impl : functor (Infer : Specs.Infer) -> sig
  include module type of Goal

  val ( <?:> ) :
       'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Bindings.t
    -> 'a Nethra_ast.Proof.t
end
