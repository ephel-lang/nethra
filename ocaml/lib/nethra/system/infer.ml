module Impl (Checker : Specs.Checker) = struct
  include Goal

  let infer _bindings _term = failwith "TODO"
  let ( <?:> ) term _ bindings = infer bindings term
end
