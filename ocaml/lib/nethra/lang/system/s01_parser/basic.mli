module Basic (Source : Nethra_syntax_source.Specs.SOURCE with type e = char) : sig
  module Parsec : Nethra_syntax_parser.Specs.PARSEC with module Source = Source

  val comment_line : string Parsec.t
  val comment_block : string Parsec.t
  val spaces : string Parsec.t
  val skip : string list Parsec.t
  val token : 'a Parsec.t -> 'a Parsec.t
  val localize : 'a Parsec.t -> ('a * Nethra_syntax_source.Region.t) Parsec.t
  val identifier : string Parsec.t
  val operator : string Parsec.t

  module Reserved : sig
    val _ARROW_ : string Parsec.t
    val _DOT_ : string Parsec.t
    val _LPAR_ : string Parsec.t
    val _RPAR_ : string Parsec.t
    val _LACC_ : string Parsec.t
    val _RACC_ : string Parsec.t
    val _COLON_ : string Parsec.t
    val _PRODUCT_ : string Parsec.t
    val _COMMA_ : string Parsec.t
    val _DISJUNCTION_ : string Parsec.t
    val _EQUAL_ : string Parsec.t
    val _SIG_ : string Parsec.t
    val _DEF_ : string Parsec.t
    val _TYPE_ : string Parsec.t
    val _CASE_ : string Parsec.t
    val _INL_ : string Parsec.t
    val _INR_ : string Parsec.t
    val _FST_ : string Parsec.t
    val _SND_ : string Parsec.t
    val _REC_ : string Parsec.t
    val _FOLD_ : string Parsec.t
    val _UNFOLD_ : string Parsec.t
    val _DATA_ : string Parsec.t
    val _LET_ : string Parsec.t
    val _IN_ : string Parsec.t
  end
end
