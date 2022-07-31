module Impl = struct
  type _ input = string
  type _ output = Nethra_toy_cst.Binding.t list
  type _ error = Nethra_syntax_source.Location.t * string

  let _render l =
    let open Nethra_syntax_source in
    let buffer = Buffer.create 16 in
    let formatter = Format.formatter_of_buffer buffer in
    let () = Location.Render.render formatter l in
    let () = Format.pp_print_flush formatter () in
    Buffer.contents buffer

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
