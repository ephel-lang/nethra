type 'a t = (string * 'a Term.t) list

module Construct = struct
  let create = []
end

module Access = struct
  let get_binding bindings n =
    Option.map snd (List.find_opt (fun c -> fst c = n) bindings)

  let add_binding bindings binding = binding :: bindings
  let add_bindings bindings more_bindings = more_bindings @ bindings
  let bindings l = l
end