module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) : sig
  val bindings : Nethra_toy_cst.Binding.t list Parsec.t
end
