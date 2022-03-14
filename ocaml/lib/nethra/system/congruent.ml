open Nethra_ast.Ast

module Impl = struct
  include Goal

  let congruent _bindings term term' =
    if term = term'
    then Proof.Builders.(congruent term term' [])
    else Proof.Builders.(congruent term term' [ failure None ])

  let ( =?= ) term term' bindings = congruent bindings term term'
end
