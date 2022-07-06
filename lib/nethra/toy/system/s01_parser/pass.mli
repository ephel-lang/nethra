module Impl :
  Nethra_lang_specs.PASS
    with type _ input = string
     and type _ output = Nethra_toy_cst.Binding.t list
     and type _ error = string
