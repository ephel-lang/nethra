module Infer = struct
  include Common

  let infer _bindings _term = failwith "TODO"
  let ( <?:> ) term _ bindings = infer bindings term
end
