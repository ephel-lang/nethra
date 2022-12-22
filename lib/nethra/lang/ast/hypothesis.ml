module Bindings = struct
  type kind =
    | Signature
    | Definition

  type 'a t = ((string * kind) * 'a Term.t) list

  module Construct = struct
    let create = []
  end

  module Access = struct
    let get_binding bindings n kind =
      Option.map snd (List.find_opt (fun c -> fst c = (n, kind)) bindings)

    let add_binding bindings (n, t) kind = ((n, kind), t) :: bindings

    let add_bindings bindings more_bindings kind =
      let more_bindings =
        List.map (fun (n, t) -> ((n, kind), t)) more_bindings
      in
      more_bindings @ bindings

    let bindings l kind =
      List.filter_map
        (fun ((n, k'), t) -> if kind = k' then Some (n, t) else None)
        l
  end
end

type 'a t = int * 'a Bindings.t

(* State monad should be use here ... *)

module Construct = struct
  let create = (0, Bindings.Construct.create)
end

module Access = struct
  let fresh_variable (r, gamma) base =
    let v = base ^ string_of_int r in
    (v, (r + 1, gamma))

  (** Signatures *)

  let signatures (_, gamma) = Bindings.Access.bindings gamma Bindings.Signature

  let get_signature (_, gamma) n =
    Bindings.Access.get_binding gamma n Bindings.Signature

  let ( @: ) r n = get_signature r n

  let add_signature (reference, gamma) signature =
    (reference, Bindings.Access.add_binding gamma signature Bindings.Signature)

  let ( +: ) r b = add_signature r b

  let add_signatures (reference, gamma) signatures =
    (reference, Bindings.Access.add_bindings gamma signatures Bindings.Signature)

  let ( +:+ ) r b = add_signatures r b

  (** Definitions *)

  let definitions (_, gamma) =
    Bindings.Access.bindings gamma Bindings.Definition

  let get_definition (_, gamma) n =
    Bindings.Access.get_binding gamma n Bindings.Definition

  let ( @= ) r n = get_definition r n

  let add_definition (reference, gamma) binding =
    (reference, Bindings.Access.add_binding gamma binding Bindings.Definition)

  let ( += ) r b = add_definition r b
end