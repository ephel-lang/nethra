module Bindings : sig
  type 'a t

  module Builders : sig
    val create : 'a t
  end

  module Access : sig
    val get_binding : 'a t -> string -> 'a Term.t option
    val add_binding : 'a t -> string * 'a Term.t -> 'a t
    val add_bindings : 'a t -> (string * 'a Term.t) list -> 'a t
  end
end

module Hypothesis : sig
  type 'a t

  module Builders : sig
    val create : 'a t
  end

  module Access : sig
    val fresh_variable : 'a t -> string -> string * 'a t
    val get_signature : 'a t -> string -> 'a Term.t option
    val add_signature : 'a t -> string * 'a Term.t -> 'a t
    val add_signatures : 'a t -> (string * 'a Term.t) list -> 'a t
    val get_definition : 'a t -> string -> 'a Term.t option
    val add_definition : 'a t -> string * 'a Term.t -> 'a t
  end
end
