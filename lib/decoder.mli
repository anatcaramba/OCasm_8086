val disassemble_file : string -> unit
(**[disassemble_file filename] opens file [filename] in binary mode to read all
   its bytes, and outputs the corresponding X8086 assembly instructions to
   standard output.*)
