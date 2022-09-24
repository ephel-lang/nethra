type 'a t

module Construct : sig
  val create : 'a t
end

module Access : sig
  val get_binding : 'a t -> string -> 'a Term.t option
  val add_binding : 'a t -> string * 'a Term.t -> 'a t
  val add_bindings : 'a t -> (string * 'a Term.t) list -> 'a t
  val bindings : 'a t -> (string * 'a Term.t) list
end