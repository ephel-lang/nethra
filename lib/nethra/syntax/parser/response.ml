type ('a, 'b) t =
  | Success of ('a * bool * 'b)
  | Failure of (string option * bool * 'b)

module Construct = struct
  let success (a, c, b) = Success (a, c, b)
  let failure (m, c, b) = Failure (m, c, b)
end

module Destruct = struct
  let fold ~success ~failure = function
    | Success (a, c, b) -> success (a, c, b)
    | Failure (m, c, b) -> failure (m, c, b)

  let fold_opt =
    let none _ = None in
    fun ?(success = none) ?(failure = none) r -> fold ~success ~failure r
end
