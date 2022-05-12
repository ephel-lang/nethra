type t = Location.t * Location.t

module Construct = struct
  let create ~first ~last = (first, last)
end

module Access = struct
  let first = fst
  let last = snd
end

module Render = struct
  let render ppf (first, last) =
    let open Format in
    let open Location.Render in
    fprintf ppf "from %a to %a" render first render last
end
