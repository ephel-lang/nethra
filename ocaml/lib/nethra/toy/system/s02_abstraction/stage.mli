module Impl :
  Nethra_lang_specs.STAGE
    with type _ input = Nethra_toy_cst.Binding.t list
     and type _ output =
      Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t
