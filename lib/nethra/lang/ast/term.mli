(* *)

type 'a t

module Construct : sig
  val kind : ?c:'a option -> int -> 'a t
  val int : ?c:'a option -> int -> 'a t
  val char : ?c:'a option -> char -> 'a t
  val string : ?c:'a option -> string -> 'a t
  val id : ?c:'a option -> ?initial:string option -> string -> 'a t
  val pi : ?c:'a option -> ?implicit:bool -> string -> 'a t -> 'a t -> 'a t
  val arrow : ?c:'a option -> 'a t -> 'a t -> 'a t
  val lambda : ?c:'a option -> ?implicit:bool -> string -> 'a t -> 'a t
  val apply : ?c:'a option -> ?implicit:bool -> 'a t -> 'a t -> 'a t
  val let_binding : ?c:'a option -> string -> 'a t -> 'a t -> 'a t
  val sigma : ?c:'a option -> string -> 'a t -> 'a t -> 'a t
  val pair : ?c:'a option -> 'a t -> 'a t -> 'a t
  val fst : ?c:'a option -> 'a t -> 'a t
  val snd : ?c:'a option -> 'a t -> 'a t
  val sum : ?c:'a option -> 'a t -> 'a t -> 'a t
  val inl : ?c:'a option -> 'a t -> 'a t
  val inr : ?c:'a option -> 'a t -> 'a t
  val case : ?c:'a option -> 'a t -> 'a t -> 'a t -> 'a t
  val mu : ?c:'a option -> string -> 'a t -> 'a t -> 'a t
  val fold : ?c:'a option -> 'a t -> 'a t
  val unfold : ?c:'a option -> 'a t -> 'a t
  val hole : ?c:'a option -> ?r:'a t option ref -> string -> 'a t
  val annotation : ?c:'a option -> 'a t -> 'a t -> 'a t
  val equals : ?c:'a option -> 'a t -> 'a t -> 'a t
  val subst : ?c:'a option -> 'a t -> 'a t -> 'a t
  val refl : ?c:'a option -> unit -> 'a t
  val record_sig : ?c:'a option -> (string * 'a t) list -> 'a t
  val record_val : ?c:'a option -> (string * 'a t) list -> 'a t
  val access : ?c:'a option -> 'a t -> string -> 'a t
end

module Destruct : sig
  val fold :
       kind:(int * 'a option -> 'b)
    -> int:(int * 'a option -> 'b)
    -> char:(char * 'a option -> 'b)
    -> string:(string * 'a option -> 'b)
    -> id:(string * string option * 'a option -> 'b)
    -> pi:(string * 'a t * 'a t * bool * 'a option -> 'b)
    -> lambda:(string * 'a t * bool * 'a option -> 'b)
    -> apply:('a t * 'a t * bool * 'a option -> 'b)
    -> let_binding:(string * 'a t * 'a t * 'a option -> 'b)
    -> sigma:(string * 'a t * 'a t * 'a option -> 'b)
    -> pair:('a t * 'a t * 'a option -> 'b)
    -> fst:('a t * 'a option -> 'b)
    -> snd:('a t * 'a option -> 'b)
    -> sum:('a t * 'a t * 'a option -> 'b)
    -> inl:('a t * 'a option -> 'b)
    -> inr:('a t * 'a option -> 'b)
    -> case:('a t * 'a t * 'a t * 'a option -> 'b)
    -> mu:(string * 'a t * 'a t * 'a option -> 'b)
    -> fold:('a t * 'a option -> 'b)
    -> unfold:('a t * 'a option -> 'b)
    -> hole:(string * 'a t option ref * 'a option -> 'b)
    -> annotation:('a t * 'a t * 'a option -> 'b)
    -> equals:('a t * 'a t * 'a option -> 'b)
    -> refl:('a option -> 'b)
    -> subst:('a t * 'a t * 'a option -> 'b)
    -> record_sig:((string * 'a t) list * 'a option -> 'b)
    -> record_val:((string * 'a t) list * 'a option -> 'b)
    -> access:('a t * string * 'a option -> 'b)
    -> 'a t
    -> 'b

  val fold_opt :
       ?kind:(int * 'a option -> 'b option)
    -> ?int:(int * 'a option -> 'b option)
    -> ?char:(char * 'a option -> 'b option)
    -> ?string:(string * 'a option -> 'b option)
    -> ?id:(string * string option * 'a option -> 'b option)
    -> ?pi:(string * 'a t * 'a t * bool * 'a option -> 'b option)
    -> ?lambda:(string * 'a t * bool * 'a option -> 'b option)
    -> ?apply:('a t * 'a t * bool * 'a option -> 'b option)
    -> ?let_binding:(string * 'a t * 'a t * 'a option -> 'b option)
    -> ?sigma:(string * 'a t * 'a t * 'a option -> 'b option)
    -> ?pair:('a t * 'a t * 'a option -> 'b option)
    -> ?fst:('a t * 'a option -> 'b option)
    -> ?snd:('a t * 'a option -> 'b option)
    -> ?sum:('a t * 'a t * 'a option -> 'b option)
    -> ?inl:('a t * 'a option -> 'b option)
    -> ?inr:('a t * 'a option -> 'b option)
    -> ?case:('a t * 'a t * 'a t * 'a option -> 'b option)
    -> ?mu:(string * 'a t * 'a t * 'a option -> 'b option)
    -> ?fold:('a t * 'a option -> 'b option)
    -> ?unfold:('a t * 'a option -> 'b option)
    -> ?hole:(string * 'a t option ref * 'a option -> 'b option)
    -> ?annotation:('a t * 'a t * 'a option -> 'b option)
    -> ?equals:('a t * 'a t * 'a option -> 'b option)
    -> ?refl:('a option -> 'b option)
    -> ?subst:('a t * 'a t * 'a option -> 'b option)
    -> ?record_sig:((string * 'a t) list * 'a option -> 'b option)
    -> ?record_val:((string * 'a t) list * 'a option -> 'b option)
    -> ?access:('a t * string * 'a option -> 'b option)
    -> ?default:('a t -> 'b option)
    -> 'a t
    -> 'b option
end
