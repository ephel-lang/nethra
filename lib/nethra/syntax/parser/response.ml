type ('a, 'b) t =
  | Success of ('a * bool * 'b)
  | Failure of (bool * 'b)

module Construct = struct
  let success (a, c, b) = Success (a, c, b)
  let failure (c, b) = Failure (c, b)
end

module Destruct = struct
  let fold ~success ~failure = function
    | Success (a, c, b) -> success (a, c, b)
    | Failure (c, b) -> failure (c, b)

  let fold_opt =
    let none _ = None in
    fun ?(success = none) ?(failure = none) r -> fold ~success ~failure r
end
