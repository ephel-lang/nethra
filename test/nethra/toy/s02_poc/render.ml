let to_string r o =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  let () = r formatter o in
  let () = Format.pp_print_flush formatter () in
  Buffer.contents buffer
