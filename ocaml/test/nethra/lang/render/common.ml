open Nethra.Render.Term

let render term =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = render formatter term in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer
