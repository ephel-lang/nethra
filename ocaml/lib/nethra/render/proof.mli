val render :
     ?term:(Format.formatter -> 'a Nethra_ast.Term.t -> unit)
  -> Format.formatter
  -> 'a Nethra_ast.Proof.t
  -> unit
