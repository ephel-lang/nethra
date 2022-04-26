module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) : sig
  val binding : unit -> Nethra_toy_cst.Binding.t Parsec.t
end
