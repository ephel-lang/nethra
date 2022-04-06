module FromList : functor (Locator : Specs.Locator) ->
  Specs.Source with type e = Locator.e and type Construct.c = Locator.e list

module FromChars :
  Specs.Source with type e = char and type Construct.c = char list
