type 'a t = 'a list * Location.t

module FromList (Locator : Specs.Locator) = struct
  type e = Locator.e
  type nonrec t = e t

  module Construct = struct
    type nonrec c = e list

    let create s = (s, Location.Construct.initial)
  end

  module Access = struct
    let next = function
      | [], l -> (None, ([], l))
      | a :: s, l ->
        (Some a, (s, Location.Construct.(combine l (Locator.locate a))))

    let eos (s, _) = s = []
    let location (_, l) = l
  end
end

module FromChars = FromList (struct
  type e = char

  let locate =
    let open Location.Construct in
    function
    | '\n' -> create ~position:1 ~line:1 ~column:0
    | _ -> create ~position:1 ~line:0 ~column:1
end)
