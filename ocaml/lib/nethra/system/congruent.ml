open Nethra_ast.Ast.Proof.Builders

module Impl = struct
  include Goal

  let congruent_terms _bindings term term' =
    if term = term' (* TODO *)
    then congruent term term' []
    else congruent term term' [ failure None ]

  let ( =?= ) (bindings, term) term' = congruent_terms bindings term term'
end
