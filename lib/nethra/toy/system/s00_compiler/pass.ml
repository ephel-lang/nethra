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

  let run s =
    let open Preface_stdlib.Result.Monad (struct
      type t = unit error
    end) in
    let open Preface_stdlib.Result.Bifunctor in
    let* cst =
      map_snd (fun e -> `SyntaxError e) (Nethra_toy_parser.Pass.run s)
    in
    let* ast =
      map_snd (fun e -> `AbstractionError e) (Nethra_toy_abstract.Pass.run cst)
    in
    let* norm_ast =
      map_snd
        (fun e -> `FreeVarsError e)
        (Nethra_lang_system_normalize.Pass.run ast)
    in
    map_snd (fun e -> `TypeError e) (Nethra_lang_system_type.Pass.run norm_ast)
end