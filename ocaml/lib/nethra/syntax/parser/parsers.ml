module Parsec (Source : Nethra_syntax_source.Specs.SOURCE) = struct
  module Source = Source

  type 'b t = Source.t -> ('b, Source.t) Response.t

  let source c = Source.Construct.create c
end

module Functor (Parsec : Specs.PARSEC) = Preface_make.Functor.Via_map (struct
  type 'a t = 'a Parsec.t

  let map f p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a, b, s) -> success (f a, b, s))
      ~failure:(fun (b, s) -> failure (b, s))
      (p s)
end)

module Monad (Parsec : Specs.PARSEC) = Preface_make.Monad.Via_bind (struct
  type 'a t = 'a Parsec.t

  let return v s =
    let open Response.Construct in
    success (v, false, s)

  let bind f p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (p, b1, s) ->
        fold
          ~success:(fun (a, b2, s) -> success (a, b1 || b2, s))
          ~failure:(fun (b, s) -> failure (b, s))
          (f p s) )
      ~failure:(fun (b, s) -> failure (b, s))
      (p s)
end)

module Eval (Parsec : Specs.PARSEC) = struct
  module Monad = Monad (Parsec)

  let locate p s =
    let open Response.Destruct in
    let open Response.Construct in
    let open Parsec.Source.Access in
    let l0 = location s in
    fold
      ~success:(fun (a, b, s) -> success ((a, l0, location s), b, s))
      ~failure:(fun (_, _) -> failure (false, s))
      (p s)

  let eos s =
    let open Response.Construct in
    let open Parsec.Source.Access in
    match next s with
    | Some _, s' -> failure (false, s')
    | None, s' -> success ((), false, s')

  let return = Monad.return

  let fail ?(consumed = false) s =
    let open Response.Construct in
    failure (consumed, s)

  let do_lazy p = p ()

  let do_try p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a, b, s) -> success (a, b, s))
      ~failure:(fun (_, _) -> failure (false, s))
      (p s)

  let lookahead p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a, _, _) -> success (a, false, s))
      ~failure:(fun (_, _) -> failure (false, s))
      (p s)

  let satisfy p f =
    let open Monad in
    do_try (p >>= fun a -> if f a then return a else fail ~consumed:false)
end

module Operator (Parsec : Specs.PARSEC) = struct
  module Functor = Functor (Parsec)
  module Eval = Eval (Parsec)

  let ( <~> ) p1 p2 s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a1, b1, s1) ->
        fold
          ~success:(fun (a2, b2, s2) -> success ((a1, a2), b1 || b2, s2))
          ~failure:(fun (b2, s2) -> failure (b1 || b2, s2))
          (p2 s1) )
      ~failure:(fun (b1, s1) -> failure (b1, s1))
      (p1 s)

  let ( <~< ) p1 p2 = Functor.(p1 <~> p2 <&> fst)
  let ( >~> ) p1 p2 = Functor.(p1 <~> p2 <&> snd)

  let ( <|> ) p1 p2 s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a, b, s) -> success (a, b, s))
      ~failure:(fun (b, s) -> if b then failure (b, s) else p2 s)
      (p1 s)

  let ( <?> ) p f =
    let open Eval in
    satisfy p f
end

module Atomic (Parsec : Specs.PARSEC) = struct
  module Monad = Monad (Parsec)
  module Eval = Eval (Parsec)
  module Operator = Operator (Parsec)

  let any s =
    let open Response.Construct in
    let open Parsec.Source.Access in
    match next s with
    | Some e, s' -> success (e, true, s')
    | None, s' -> failure (false, s')

  let not p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (_, _, s) -> failure (false, s))
      ~failure:(fun (_, s) -> any s)
      (p s)

  let atom e =
    let open Operator in
    any <?> fun e' -> e' = e

  let atom_in l =
    let open Operator in
    any <?> fun e' -> List.mem e' l

  let atoms l =
    let open List in
    let open Monad in
    let open Eval in
    let open Operator in
    do_try
      (fold_left (fun p e -> p <~< atom e) (return ()) l <&> Stdlib.Fun.const l)
end

module Occurrence (Parsec : Specs.PARSEC) = struct
  module Monad = Monad (Parsec)
  module Eval = Eval (Parsec)
  module Operator = Operator (Parsec)

  let opt p s =
    let open Response.Destruct in
    let open Response.Construct in
    fold
      ~success:(fun (a, b, s) -> success (Some a, b, s))
      ~failure:(fun (b, s) -> if b then failure (b, s) else success (None, b, s))
      (p s)

  let sequence optional p s =
    (* sequence is tail recursive *)
    let open Response.Destruct in
    let open Response.Construct in
    let rec sequence s aux b =
      fold
        ~success:(fun (a, b', s') -> sequence s' (a :: aux) (b || b'))
        ~failure:(fun (b', s') ->
          if b' || (aux = [] && not optional)
          then failure (b || b', s')
          else success (List.rev aux, b || b', s) )
        (p s)
    in
    sequence s [] false

  let rep p = sequence false p
  let opt_rep p = sequence true p
end

module Literal (Parsec : Specs.PARSEC with type Source.e = char) = struct
  module Monad = Monad (Parsec)
  module Atomic = Atomic (Parsec)
  module Operator = Operator (Parsec)
  module Occurrence = Occurrence (Parsec)

  let char c =
    let open Operator in
    let open Atomic in
    any <?> fun e' -> e' = c

  let char_in_range (l, u) =
    let open Operator in
    let open Atomic in
    any <?> fun e' -> l <= e' && e' <= u

  let char_in_list l =
    let open Operator in
    let open Atomic in
    any <?> fun e' -> List.mem e' l

  let char_in_string s =
    let open Nethra_syntax_source.Utils in
    char_in_list (chars_of_string s)

  let digit = char_in_range ('0', '9')

  let alpha =
    let open Operator in
    char_in_range ('a', 'z') <|> char_in_range ('A', 'Z')

  let natural =
    let open Monad in
    let open Occurrence in
    let open Nethra_syntax_source.Utils in
    rep digit <&> string_of_chars <&> int_of_string

  let integer =
    let open Monad in
    let open Atomic in
    let open Operator in
    let open Occurrence in
    atom '-'
    >~> return (( * ) (-1))
    <|> (opt (atom '+') >~> return Stdlib.Fun.id)
    <~> natural
    <&> fun (f, i) -> f i

  let string s =
    let open Monad in
    let open Atomic in
    let open Nethra_syntax_source.Utils in
    atoms (chars_of_string s) <&> Stdlib.Fun.const s

  let sequence p =
    let open Monad in
    let open Occurrence in
    let open Nethra_syntax_source.Utils in
    rep p <&> string_of_chars

  module Delimited = struct
    let string_delimited =
      let open Monad in
      let open Atomic in
      let open Operator in
      let open Occurrence in
      let open Nethra_syntax_source.Utils in
      char '"'
      >~> opt_rep
            (char '\\' >~> char '"' <&> Stdlib.Fun.const '"' <|> not (char '"'))
      <~< char '"'
      <&> string_of_chars

    let char_delimited =
      let open Monad in
      let open Atomic in
      let open Operator in
      char '\''
      >~> (string "\\\'" <&> Stdlib.Fun.const '\'' <|> not (char '\''))
      <~< char '\''

    let string = string_delimited
    let char = char_delimited
  end
end
