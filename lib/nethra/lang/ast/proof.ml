(* *)

type 'a goal =
  | Check of 'a Hypothesis.t * 'a Term.t * 'a Term.t
  | Infer of 'a Hypothesis.t * 'a Term.t * 'a Term.t option
  | Equivalent of 'a Hypothesis.t * 'a Term.t * 'a Term.t

type 'a t =
  | Step of 'a goal * 'a t list
  | Fail of string option

module Construct = struct
  let check hypothesis term kind steps =
    Step (Check (hypothesis, term, kind), steps)

  let infer hypothesis term kind steps =
    Step (Infer (hypothesis, term, kind), steps)

  let equivalent hypothesis term kind steps =
    Step (Equivalent (hypothesis, term, kind), steps)

  let failure reason = Fail reason
end

module Destruct = struct
  let fold ~check ~infer ~equivalent ~failure = function
    | Step (Check (hypothesis, term, kind), steps) ->
      check (hypothesis, term, kind, steps)
    | Step (Infer (hypothesis, term, kind), steps) ->
      infer (hypothesis, term, kind, steps)
    | Step (Equivalent (hypothesis, lhd, rhd), steps) ->
      equivalent (hypothesis, lhd, rhd, steps)
    | Fail reason -> failure reason

  let get_type p =
    fold
      ~check:(fun (_, _, kind, _) -> Some kind)
      ~infer:(fun (_, _, kind, _) -> kind)
      ~equivalent:(fun (_, _, _, _) -> None)
      ~failure:(fun _ -> None)
      p
end

let rec is_success step =
  Destruct.fold
    ~check:(fun (_, _, _, steps) -> List.for_all is_success steps)
    ~infer:(fun (_, _, _, steps) -> List.for_all is_success steps)
    ~equivalent:(fun (_, _, _, steps) -> List.for_all is_success steps)
    ~failure:(fun _ -> false)
    step

let rec size step =
  1
  + Destruct.fold
      ~check:(fun (_, _, _, steps) ->
        List.fold_left (fun s e -> s + size e) 0 steps )
      ~infer:(fun (_, _, _, steps) ->
        List.fold_left (fun s e -> s + size e) 0 steps )
      ~equivalent:(fun (_, _, _, steps) ->
        List.fold_left (fun s e -> s + size e) 0 steps )
      ~failure:(fun _ -> 0)
      step
