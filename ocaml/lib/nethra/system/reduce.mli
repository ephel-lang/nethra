module Impl : sig
  val reduce :
    'a Nethra_ast.Bindings.t -> 'a Nethra_ast.Term.t -> 'a Nethra_ast.Term.t
end
