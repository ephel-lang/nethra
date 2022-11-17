type 'a t

module Construct : sig
  val create : 'a t
end

module Access : sig
  val fresh_variable : 'a t -> string -> string * 'a t

  (** Signatures *)

  val signatures : 'a t -> (string * 'a Term.t) list
  val get_signature : 'a t -> string -> 'a Term.t option
  val add_signature : 'a t -> string * 'a Term.t -> 'a t
  val add_signatures : 'a t -> (string * 'a Term.t) list -> 'a t

  (** Definitions *)

  val definitions : 'a t -> (string * 'a Term.t) list
  val get_definition : 'a t -> string -> 'a Term.t option
  val add_definition : 'a t -> string * 'a Term.t -> 'a t
end
