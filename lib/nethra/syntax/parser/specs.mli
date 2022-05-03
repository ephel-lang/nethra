module Parsec : sig
  module Source : Nethra_syntax_source.Specs.SOURCE

  type 'a t = Source.t -> ('a, Source.t) Response.t

  val source : Source.Construct.c -> Source.t
end

module type PARSEC = module type of Parsec
