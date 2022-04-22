open Nethra.Syntax.Source
open Nethra.Syntax.Parser

let response r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some a, b))
    ~failure:(fun (b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)
