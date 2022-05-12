type t = int * int * int

module Construct = struct
  (* Monoid neutral *)
  let initial = (0, 0, 0)

  (* Monoid combine *)
  let combine (position, line, column) (position', line', column') =
    (position + position', line + line', column + column')

  let create ~position ~line ~column = (position, line, column)
end

module Access = struct
  let position (p, _, _) = p
  let line (_, l, _) = l
  let column (_, _, c) = c
end

module Render = struct
  let render ppf (_, l, c) =
    let open Format in
    fprintf ppf "%d:%d" l c
end
