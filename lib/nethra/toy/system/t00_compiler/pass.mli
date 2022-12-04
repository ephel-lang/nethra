module Impl :
  Nethra_lang_specs.PASS
    with type _ input = string
     and type _ output =
      Nethra_syntax_source.Region.t Nethra_lang_ast.Hypothesis.t
      * (string * Nethra_syntax_source.Region.t Nethra_lang_ast.Proof.t option)
        list
     and type _ error =
      [ `SyntaxError of unit Nethra_toy_parser.Pass.error
      | `AbstractionError of unit Nethra_toy_abstract.Pass.error
      | `FreeVarsError of
        Nethra_syntax_source.Region.t Nethra_lang_system_normalize.Pass.error
      | `TypeError of
        Nethra_syntax_source.Region.t Nethra_lang_system_type.Pass.error
      ]
