val ( ?? ) :
  'a Nethra_ast.Term.t -> 'a Nethra_ast.Bindings.t -> 'a Nethra_ast.Proof.t

val ( ?: ) :
     'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Term.t
  -> 'a Nethra_ast.Bindings.t
  -> 'a Nethra_ast.Proof.t

val ( |- ) : 'a Nethra_ast.Bindings.t -> ('a Nethra_ast.Bindings.t -> 'b) -> 'b
