module L0 (Source : Nethra_syntax_source.Specs.SOURCE with type e = char) : sig
  module Parsec : Nethra_syntax_parser.Specs.PARSEC with module Source = Source

  val comment_line : string Parsec.t
  val comment_block : string Parsec.t
  val spaces : string Parsec.t
  val skip : string list Parsec.t
  val token : 'a Parsec.t -> 'a Parsec.t
  val localize : 'a Parsec.t -> ('a * Nethra_syntax_source.Region.t) Parsec.t
end
