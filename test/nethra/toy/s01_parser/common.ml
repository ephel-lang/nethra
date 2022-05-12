open Nethra.Syntax.Source
open Nethra.Syntax.Parser
open Nethra.Toy.Cst.Render

let response f r =
  let open Response.Destruct in
  fold
    ~success:(fun (a, b, _) -> (Some (f a), b))
    ~failure:(fun (_, b, _) -> (None, b))
    r

module Parsec = Parsers.Parsec (Sources.FromChars)

let render term =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = render_localized formatter term in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer

let render_binding term =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = render_binding formatter term in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer

let rec render_bindings = function
  | [] -> ""
  | [ a ] -> render_binding a
  | a :: l -> render_binding a ^ " " ^ render_bindings l
