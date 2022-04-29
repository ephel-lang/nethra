module Impl :
  Nethra_lang_specs.STAGE
    with type input = Nethra_toy_cst.Binding.t list
     and type output =
      Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t
