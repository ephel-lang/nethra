type t = Location.t * Location.t

module Construct = struct
  let create ~first ~last = (first, last)
end

module Access = struct
  let first = fst
  let last = snd
end
