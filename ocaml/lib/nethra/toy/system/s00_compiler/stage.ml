module Impl = struct
  type _ input = string

  type _ output =
    ( Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t
      * (string * Nethra_syntax_source.Region.t Nethra_lang_ast.Proof.t option)
        list
    , string )
    Preface_stdlib.Result.t

  let run input =
    let open Preface_stdlib.Result.Monad (struct
      type t = string
    end) in
    Nethra_toy_parser.Stage.run input
    <&> Nethra_toy_abstractor.Stage.run
    <&> Nethra_lang_system_type.Stage.run
end
