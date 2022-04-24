module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) : sig
  val term : unit -> Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
end
