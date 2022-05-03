val render :
     ?term:(Format.formatter -> 'a Nethra_lang_ast.Term.t -> unit)
  -> Format.formatter
  -> 'a Nethra_lang_ast.Proof.t
  -> unit
