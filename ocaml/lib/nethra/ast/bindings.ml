type 'a dictionary = (string * 'a) list
type 'a t = int ref * 'a Term.t dictionary * 'a Term.t dictionary

(* State monad should be use here ... *)

module Builders = struct
  let create = (ref 0, [], [])
end

module Access = struct
  let fresh_variable (r, gamma, omega) base =
    let v = base ^ string_of_int !r in
    let () = r := !r + 1 in
    (v, (r, gamma, omega))

  let get_signature (_, gamma, _) n =
    Option.map snd (List.find_opt (fun c -> fst c = n) gamma)

  let add_signature (reference, gamma, delta) binding =
    (reference, binding :: gamma, delta)

  let add_signatures (reference, gamma, delta) signatures =
    (reference, signatures @ gamma, delta)

  let get_definition (_, _, delta) n =
    Option.map snd (List.find_opt (fun c -> fst c = n) delta)

  let add_definition (reference, gamma, delta) binding =
    (reference, gamma, binding :: delta)
end
