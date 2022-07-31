module Impl = struct
  type _ input = string
  type _ output = Nethra_toy_cst.Binding.t list
  type _ error = Nethra_syntax_source.Location.t * string

  let run input =
    let open Preface_stdlib.Result in
    let open Nethra_syntax_parser in
    let open Nethra_syntax_source in
    let module Parsec = Parsers.Parsec (Sources.FromChars) in
    let module Bindings = Bindings.Impl (Parsec) in
    Response.Destruct.fold
      ~success:(fun (r, _, _) -> Ok r)
      ~failure:(fun (m, _, s) ->
        let message = function None -> "" | Some m -> ": " ^ m in
        Error (Sources.FromChars.Access.location s, message m) )
      (Bindings.bindings @@ Parsec.source @@ Utils.chars_of_string input)
end
