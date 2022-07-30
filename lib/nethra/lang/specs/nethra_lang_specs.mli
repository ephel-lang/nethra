module Pass : sig
  type 'a input
  type 'a output
  type 'a error

  val run : 'a input -> ('a output, 'a error) Result.t
end

module type PASS = module type of Pass
