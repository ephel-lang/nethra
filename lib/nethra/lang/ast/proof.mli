(* *)

type 'a t

module Construct : sig
  val check : 'a Hypothesis.t -> 'a Term.t -> 'a Term.t -> 'a t list -> 'a t

  val infer :
    'a Hypothesis.t -> 'a Term.t -> 'a Term.t option -> 'a t list -> 'a t

  val equivalent :
    'a Hypothesis.t -> 'a Term.t -> 'a Term.t -> 'a t list -> 'a t

  val failure : string option -> 'a t
end

module Destruct : sig
  val fold :
       check:('a Hypothesis.t * 'a Term.t * 'a Term.t * 'a t list -> 'b)
    -> infer:('a Hypothesis.t * 'a Term.t * 'a Term.t option * 'a t list -> 'b)
    -> equivalent:('a Hypothesis.t * 'a Term.t * 'a Term.t * 'a t list -> 'b)
    -> failure:(string option -> 'b)
    -> 'a t
    -> 'b

  val get_type : 'a t -> 'a Term.t option
end

val is_success : 'a t -> bool
val size : 'a t -> int