val render :
     ?_term_render:(Format.formatter -> 'a Nethra_lang_ast.Term.t -> unit)
  -> Format.formatter
  -> 'a Nethra_lang_ast.Hypothesis.t
  -> unit
