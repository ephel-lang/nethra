type t = string option * Location.t * Location.t

module Construct = struct
  let create ?(file = None) first last = (file, first, last)
end

module Access = struct
  let file (file, _, _) = file
  let first (_, first, _) = first
  let last (_, _, last) = last
end

module Render = struct
  let render ppf (file, first, last) =
    let open Format in
    let open Location.Render in
    match file with
    | Some file ->
      fprintf ppf "in %s from %a to %a" file render first render last
    | None -> fprintf ppf "from %a to %a" render first render last
end
