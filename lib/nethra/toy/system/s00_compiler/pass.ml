module Impl = struct
  open Nethra_syntax_source
  open Nethra_lang_ast
  module Parser = Nethra_toy_parser.Pass
  module Abstraction = Nethra_toy_abstract.Pass
  module Normalization = Nethra_lang_system_normalize.Pass
  module Type_checker = Nethra_lang_system_type.Pass

  type _ input = string

  type _ output =
    Region.t Hypothesis.t * (string * Region.t Proof.t option) list

  type _ error =
    [ `SyntaxError of unit Parser.error
    | `AbstractionError of unit Abstraction.error
    | `FreeVarsError of Region.t Normalization.error
    | `TypeError of Region.t Type_checker.error
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
    Parser.run %> map_snd syntax_error
    >=> Abstraction.run %> map_snd abstraction_error
    >=> Normalization.run %> map_snd freevars_error
    >=> Type_checker.run %> map_snd type_error
end
