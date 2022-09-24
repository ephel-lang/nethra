module Bindings = struct
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
end

type 'a t = int * 'a Bindings.t * 'a Bindings.t * 'a Bindings.t

(* State monad should be use here ... *)

module Construct = struct
  let create =
    ( 0
    , Bindings.Construct.create
    , Bindings.Construct.create
    , Bindings.Construct.create )
end

module Access = struct
  let fresh_variable (r, gamma, omega, subst) base =
    let v = base ^ string_of_int r in
    (v, (r + 1, gamma, omega, subst))

  (** Signatures *)

  let signatures (_, gamma, _, _) = Bindings.Access.bindings gamma
  let get_signature (_, gamma, _, _) n = Bindings.Access.get_binding gamma n

  let add_signature (reference, gamma, delta, subst) binding =
    (reference, Bindings.Access.add_binding gamma binding, delta, subst)

  let add_signatures (reference, gamma, delta, subst) signatures =
    (reference, Bindings.Access.add_bindings gamma signatures, delta, subst)

  (** Definitions *)

  let definitions (_, _, delta, _) = Bindings.Access.bindings delta
  let get_definition (_, _, delta, _) n = Bindings.Access.get_binding delta n

  let add_definition (reference, gamma, delta, subst) binding =
    (reference, gamma, Bindings.Access.add_binding delta binding, subst)

  (** Substitutions *)

  let substitutions (_, _, _, subst) = Bindings.Access.bindings subst
  let get_substitution (_, _, _, subst) n = Bindings.Access.get_binding subst n

  let add_substitution (reference, gamma, delta, subst) binding =
    (reference, gamma, delta, Bindings.Access.add_binding subst binding)
end