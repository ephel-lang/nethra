module Impl :
  Nethra_lang_specs.PASS
    with type 'a input = 'a Nethra_lang_ast.Context.Hypothesis.t
     and type 'a output =
      'a Nethra_lang_ast.Context.Hypothesis.t
      * (string * 'a Nethra_lang_ast.Proof.t option) list
     and type _ error = string
