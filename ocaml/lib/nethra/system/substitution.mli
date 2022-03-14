module Impl : sig
  val substitute :
       string
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
    -> 'a Nethra_ast.Term.t
end
