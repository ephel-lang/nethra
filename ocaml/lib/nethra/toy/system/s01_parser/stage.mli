module Impl :
  Nethra_lang_specs.STAGE
    with type input = string
     and type output = (Nethra_toy_cst.Binding.t list, string) Result.t
