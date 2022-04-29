module Stage : sig
  type input
  type output

  val run : input -> output
end

module type STAGE = module type of Stage
