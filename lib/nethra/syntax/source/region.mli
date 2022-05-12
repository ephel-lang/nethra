type t

module Construct : sig
  val create : first:Location.t -> last:Location.t -> t
end

module Access : sig
  val first : t -> Location.t
  val last : t -> Location.t
end

module Render : sig
  val render : Format.formatter -> t -> unit
end
