module Impl (Checker : Specs.Checker) = struct
  include Goal

  let infer _bindings _term = failwith "TODO"
  let ( <?:> ) (bindings, term) _ = infer bindings term
end
