type t = string option * Location.t * Location.t

module Construct = struct
  let create ?(file = None) first last = (file, first, last)
end

module Access = struct
  let file (file, _, _) = file
  let first (_, first, _) = first
  let last (_, _, second) = second
end

module Render = struct
  let render ppf (_, first, last) =
    let open Format in
    let open Location.Render in
    fprintf ppf "from %a to %a" render first render last
end
