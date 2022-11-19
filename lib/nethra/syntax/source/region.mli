type t

module Construct : sig
  val create : ?file:string option -> Location.t -> Location.t -> t
end

module Access : sig
  val file : t -> string option
  val first : t -> Location.t
  val last : t -> Location.t
end

module Render : sig
  val render : Format.formatter -> t -> unit
end
