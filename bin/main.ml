(** Contains the register represented by REG 
    (or R/M if MOD is 0b11),
    at position W * 8 + REG*)
let reg_field_encoding =
    [|
      "al";
      "cl";
      "dl";
      "bl";
      "ah";
      "ch";
      "dh";
      "bh";
      "ax";
      "cx";
      "dx";
      "bx";
      "sp";
      "bp";
      "si";
      "di";
    |]

let decode_reg reg_val w = reg_field_encoding.((w * 8) + reg_val)

let disassemble_mov_reg_reg in_c =
    let a = input_byte in_c in
    let b = input_byte in_c in
    let opcode = a lsr 2 in
    let mov_rr_opcode = 0x22 in
    if opcode <> mov_rr_opcode then
      failwith "Specified opcode is not as expected. Aborting.";
    let d, w = ((a lsr 1) land 1, a land 1) in
    let mod_val, reg, rm = (b lsr 6, (b lsr 3) land 0x07, b land 0x07) in
    if mod_val <> 0x3 then
      failwith "Instruction not in register mode. Aborting.";
    (* Printf.printf "a=%d ; b=%d ; opcode=%d ; mov_rr_opcode=%d\n" a b opcode *)
    (*   mov_rr_opcode; *)
    (* Printf.printf "d=%d ; w=%d ; mod_val=%d ; reg=%d ; rm=%d\n" d w mod_val reg *)
    (*   rm; *)
    print_string "mov ";
    let reg_name = decode_reg reg w in
    let rm_name = decode_reg rm w in
    Printf.printf "%s, %s\n"
      (if d = 0 then rm_name else reg_name)
      (if d = 0 then reg_name else rm_name)

let disassemble_file filename =
    print_endline "bits 16\n";
    let in_c = open_in_bin filename in
    try
      while true do
        disassemble_mov_reg_reg in_c
      done
    with End_of_file -> ()

let () =
    if Array.length Sys.argv <= 1 then
      failwith "No file name specified. Aborting."
    else disassemble_file Sys.argv.(1)
