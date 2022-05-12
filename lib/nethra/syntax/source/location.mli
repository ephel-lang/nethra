type t

module Construct : sig
  val initial : t
  val combine : t -> t -> t
  val create : position:int -> line:int -> column:int -> t
end

module Access : sig
  val position : t -> int
  val line : t -> int
  val column : t -> int
end

module Render : sig
  val render : Format.formatter -> t -> unit
end
