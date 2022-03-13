module Infer = struct
  include Goal

  let infer _bindings _term = failwith "TODO"
  let ( <?:> ) term _ bindings = infer bindings term
end
