module Pass : sig
  type 'a input
  type 'a output

  val run : 'a input -> ('a output, string) Result.t
end

module type PASS = module type of Pass
