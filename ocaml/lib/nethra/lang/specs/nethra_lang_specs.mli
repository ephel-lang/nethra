module Stage : sig
  type 'a input
  type 'a output

  val run : 'a input -> 'a output
end

module type STAGE = module type of Stage
