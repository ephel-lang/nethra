module Impl = struct
  type _ input = string
  type _ output = (Nethra_toy_cst.Binding.t list, string) Result.t

  let run input =
    let open Nethra_syntax_parser in
    let open Nethra_syntax_source in
    let module Parsec = Parsers.Parsec (Sources.FromChars) in
    let module Bindings = Bindings.Impl (Parsec) in
    Response.Destruct.fold
      (Bindings.bindings @@ Parsec.source @@ Utils.chars_of_string input)
      ~success:(fun (r, _, _) -> Ok r)
      ~failure:(fun (_, _) -> Error "Cannot parse ... TODO")
end
