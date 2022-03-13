let congruent _bindings _term _term' = failwith "TODO"
let ( =?= ) term term' bindings = congruent bindings term term'
let ( |- ) bindings f = f bindings
