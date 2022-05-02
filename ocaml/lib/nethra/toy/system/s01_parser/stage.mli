module Impl :
  Nethra_lang_specs.STAGE
    with type _ input = string
     and type _ output =
      (Nethra_toy_cst.Binding.t list, string) Preface_stdlib.Result.t
