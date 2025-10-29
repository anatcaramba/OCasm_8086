open Theproc

let () =
    if Array.length Sys.argv <= 1 then
      failwith "No file name specified. Aborting."
    else Decoder.disassemble_file Sys.argv.(1)
