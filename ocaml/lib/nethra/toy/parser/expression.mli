module Impl
    (Parsec : Nethra_syntax_parser.Specs.PARSEC with type Source.e = char) : sig
  val kind : Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
  val var : Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
  val int : Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
  val string : Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
  val char : Nethra_toy_cst.Term.t Nethra_toy_cst.Localized.t Parsec.t
end
