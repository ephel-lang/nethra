module type Locator = sig
  type e

  val locate : e -> Location.t
end

module type Source = sig
  type e
  type t

  module Construct : sig
    type c

    val create : c -> t
  end

  module Access : sig
    val next : t -> e option * t
    val eos : t -> bool
    val location : t -> Location.t
  end
end
