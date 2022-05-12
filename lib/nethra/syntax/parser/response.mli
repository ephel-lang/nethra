type (_, _) t

module Construct : sig
  val success : 'a * bool * 'b -> ('a, 'b) t
  val failure : string option * bool * 'b -> ('a, 'b) t
end

module Destruct : sig
  val fold :
       success:('a * bool * 'b -> 'c)
    -> failure:(string option * bool * 'b -> 'c)
    -> ('a, 'b) t
    -> 'c

  val fold_opt :
       ?success:('a * bool * 'b -> 'c option)
    -> ?failure:(string option * bool * 'b -> 'c option)
    -> ('a, 'b) t
    -> 'c option
end
