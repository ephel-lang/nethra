type (_, _) t

module Construct : sig
  val success : 'a * bool * 'b -> ('a, 'b) t
  val failure : bool * 'b -> ('a, 'b) t
end

module Destruct : sig
  val fold :
       success:('a * bool * 'b -> 'c)
    -> failure:(bool * 'b -> 'c)
    -> ('a, 'b) t
    -> 'c

  val fold_opt :
       ?success:('a * bool * 'b -> 'c option)
    -> ?failure:(bool * 'b -> 'c option)
    -> ('a, 'b) t
    -> 'c option
end
