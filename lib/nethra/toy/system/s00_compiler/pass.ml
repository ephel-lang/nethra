module Impl = struct
  type _ input = string

  type _ output =
    Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t
    * (string * Nethra_syntax_source.Region.t Nethra_lang_ast.Proof.t option)
      list

  type _ error = string

  let run =
    let open Preface_stdlib.Result.Monad (struct
      type t = string
    end) in
    Nethra_toy_parser.Pass.run
    >=> Nethra_toy_desugar.Pass.run
    >=> Nethra_lang_system_type.Pass.run
end