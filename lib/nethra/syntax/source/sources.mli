module FromList : functor (Locator : Specs.LOCATOR) ->
  Specs.SOURCE with type e = Locator.e and type Construct.c = Locator.e list

module FromChars :
  Specs.SOURCE with type e = char and type Construct.c = char list
