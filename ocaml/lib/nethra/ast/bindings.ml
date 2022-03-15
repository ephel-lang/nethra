type 'a dictionary = (string * 'a) list
type 'a t = 'a Term.t dictionary * 'a Term.t dictionary

module Builders = struct
  let create = ([], [])
end

module Access = struct
  let get_signature (gamma, _) n =
    Option.map snd (List.find_opt (fun c -> fst c = n) gamma)

  let add_signature (gamma, delta) binding = (binding :: gamma, delta)

  let get_definition (_, delta) n =
    Option.map snd (List.find_opt (fun c -> fst c = n) delta)

  let add_definition (gamma, delta) binding = (gamma, binding :: delta)
end
