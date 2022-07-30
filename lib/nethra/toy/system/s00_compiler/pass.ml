module Impl = struct
  type _ input = string

  type _ output =
    Nethra_syntax_source.Region.t Nethra_lang_ast.Context.Hypothesis.t
    * (string * Nethra_syntax_source.Region.t Nethra_lang_ast.Proof.t option)
      list

  type _ error =
    [ `SyntaxError of unit Nethra_toy_parser.Pass.error
    | `AbstractionError of unit Nethra_toy_abstract.Pass.error
    | `FreeVarsError of
      Nethra_syntax_source.Region.t Nethra_lang_system_normalize.Pass.error
    | `TypeError of
      Nethra_syntax_source.Region.t Nethra_lang_system_type.Pass.error
    ]

  let run =
    let open Preface_stdlib.Result.Monad (struct
      type t = unit error
    end) in
    let open Preface_stdlib.Result.Bifunctor in
    let open Preface_core.Fun in
    let syntax_error e = `SyntaxError e
    and abstraction_error e = `AbstractionError e
    and freevars_error e = `FreeVarsError e
    and type_error e = `TypeError e in
    Nethra_toy_parser.Pass.run %> map_snd syntax_error
    >=> Nethra_toy_abstract.Pass.run %> map_snd abstraction_error
    >=> Nethra_lang_system_normalize.Pass.run %> map_snd freevars_error
    >=> Nethra_lang_system_type.Pass.run %> map_snd type_error
end