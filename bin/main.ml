(** Contains the register represented by [reg_val] (or [rm_val] if [mod] is
    0b11), at position [w] * 8 + [reg_val] ([w] being 0 or 1). Has length 16.*)
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

(* Aliases for literal opcode byte values.
   The byte contains 0 where it is not specified for given opcode.*)

(** Opcode value for "register/memory to/from register".*)
let mov_reg_tofr_rm_opcode = 0x88

(** Opcode value for "immediate to register/memory".*)
let mov_imm_to_reg_opcode = 0xB0

(** Opcode value for "immediate to register".*)
let mov_imm_to_rm_opcode = 0xC6

(** Opcode value for "memory to accumulator".*)
let mov_mem_to_acc_opcode = 0xA0

(** Opcode value for "accumulator to memory".*)
let mov_acc_to_mem_opcode = 0xA2

(** Type representing all different kinds of opcodes. Allows to use
    pattern-matching over opcodes, along with function [byte_to_opcode]. The
    latter must remain, as much as possible, the only way to read directly an
    opcode's integer value. This helps ensure all opcodes are tested and matched
    while avoiding multiple literal values in source code.*)
type opcode_t =
    | Mov_reg_tofr_rm_opcode
        (** Opcode variant for "register/memory to/from register".*)
    | Mov_imm_to_rm_opcode
        (** Opcode variant for "immediate to register/memory".*)
    | Mov_imm_to_reg_opcode  (** Opcode variant for "immediate to register".*)
    | Mov_mem_to_acc_opcode  (** Opcode variant for "memory to accumulator".*)
    | Mov_acc_to_mem_opcode  (** Opcode variant for "accumulator to memory".*)
    | Wrong_opcode  (** Opcode variant for invalid opcodes.*)

(** Returns true iff [word] matches [target] when applying [mask].*)
let are_equal_mask word target mask = word land mask = target land mask

(** Returns the register name corresponding to [reg_val] and width [w].*)
let reg_decode reg_val w = reg_field_encoding.((w * 8) + reg_val)

(** Returns value obtaines when shifting [word] to the right by [shift] bits,
    then applying [mask].*)
let field word shift mask = (word lsr shift) land mask

(** Computes opcodes on one byte, ignoring d and w values, not shifted*)
(* let opcode_to_value = function *)
(*     | Mov_reg_tofr_rm_opcode -> mov_reg_tofr_rm_opcode *)
(*     | Mov_imm_to_reg_opcode -> mov_imm_to_reg_opcode *)
(*     | Mov_imm_to_rm_opcode -> mov_imm_to_rm_opcode *)
(*     | Mov_mem_to_acc_opcode -> mov_mem_to_acc_opcode *)
(*     | Mov_acc_to_mem_opcode -> mov_acc_to_mem_opcode *)
(*     | Wrong_opcode -> raise (Invalid_argument "wrong opcode") *)

(**Given [byte], tries to match it with a valid opcode. For each opcode, only
   the corresponding bits are tested, hence a specific mask is provided for
   every opcode.*)
let byte_to_opcode byte =
    let mask_7, mask_6, mask_5, mask_4 = (0XFE, 0XFC, 0XF8, 0XF0) in
    let _ = mask_5 in
    if are_equal_mask byte mov_reg_tofr_rm_opcode mask_6 then
      Mov_reg_tofr_rm_opcode
    else if are_equal_mask byte mov_imm_to_rm_opcode mask_7 then
      Mov_imm_to_rm_opcode
    else if are_equal_mask byte mov_imm_to_reg_opcode mask_4 then
      Mov_imm_to_reg_opcode
    else if are_equal_mask byte mov_mem_to_acc_opcode mask_7 then
      Mov_mem_to_acc_opcode
    else if are_equal_mask byte mov_acc_to_mem_opcode mask_7 then
      Mov_acc_to_mem_opcode
    else Wrong_opcode

(** Type representing all possible MOD fields. Allows to use pattern-matching
    over them, along with function {!byte_to_mod}. The latter must remain, as
    much as possible, the only way to read directly a mod field's integer value.
    This helps ensure all possibilities are tested and matched while avoiding
    multiple literal values in source code.*)
type mod_t =
    | Mem_mod_no_disp  (** Mod variant for Memory mode with no displacement*)
    | Mem_mod_8_disp  (** Mod variant for Memory mode with 8-bit displacement*)
    | Mem_mod_16_disp
        (** Mod variant for Memory mode with 16-bit displacement*)
    | Reg_mod  (** Mod variant for Register mode (no displacement)*)
    | Wrong_mod  (** Mod variant for invalid mod_t field value.*)

(* Aliases for literal mod field values.*)

(** Mod value for Memory mode with no displacement*)
let mem_mod_no_disp = 0x0

(** Mod value for Memory mode with 8-bit displacement*)
let mem_mod_8_disp = 0x1

(** Mod value for Memory mode with 16-bit displacement*)
let mem_mod_16_disp = 0x2

(** Mod value for Register mode (no displacement)*)
let reg_mod = 0x3

(**Given [byte], tries to match it with a valid mod field. Only the two least
   significant bits are checked. This means the given byte must be already
   shifted, depending on the instruction.*)
let byte_to_mod byte =
    let mask = 0x3 in
    if are_equal_mask byte mem_mod_no_disp mask then Mem_mod_no_disp
    else if are_equal_mask byte mem_mod_8_disp mask then Mem_mod_8_disp
    else if are_equal_mask byte mem_mod_16_disp mask then Mem_mod_16_disp
    else if are_equal_mask byte reg_mod mask then Reg_mod
    else Wrong_mod

(** [is_two_reg_rm_field rm_val] returns [true] iff [rm_val] field represents a
    two register sum (with displacement or not).*)
let is_two_reg_rm_field rm_val = rm_val < 0x4

(** [is_dir_add_rm_field rm_val] returns [true] iff [rm_val] field represents a
    direct address calculation (in case MOD field indicates so).*)
let is_dir_add_rm_field rm_val = rm_val = 0x6

(** In the case of effective address calculation, if [rm_val] is the value of
    the RM field and represents the sum of two registers (with or without
    displacement), returns both registers' names.

    Raises {!Invalid_argument} if [rm_val] is not a valid two-register R/M
    value.*)
let decode_eff_add_2 rm_val =
    if rm_val = 0x00 then ("bx", "si")
    else if rm_val = 0x01 then ("bx", "di")
    else if rm_val = 0x02 then ("bp", "si")
    else if rm_val = 0x03 then ("bp", "di")
    else
      raise
      @@ Invalid_argument
           "rm_val does not represent a two-register effective address \
            calculation"

(** In the case of effective address calculation, if [rm_val] is the value of
    the RM field and represents the sum of one register, and a potential
    displacement, returns the register's name.

    This does not take the special case of direct address calculation into
    account.

    Raises {!Invalid_argument} if [rm_val] is not a valid one-register R/M
    value.*)
let decode_eff_add_1 rm_val =
    if rm_val = 0x04 then "si"
    else if rm_val = 0x05 then "di"
    else if rm_val = 0x06 then "bp"
    else if rm_val = 0x07 then "bx"
    else
      raise
      @@ Invalid_argument
           "rm_val does not represent a one-register effective address \
            calculation"

(** [convert_disp_signed_8 disp_lo] changes unsigned byte value [disp_lo] and
    converts it into the signed 2-complement value represented. It then returns
    the pair ([sign_char], [abs_val]) where [sign_char] is ['-'] or ['+'], the
    sign of the signed value, and [abs_val] is its absolute value. [disp_lo] is
    expected to fit in a byte.*)
let convert_disp_signed_8 disp_lo =
    if disp_lo >= 0x80 then ('-', 0x100 - disp_lo) else ('+', disp_lo)

(** [convert_disp_signed_16 disp_lo disp_hi] computes the value represented by
    the two displacement bytes in signed 2-complement representation. It then
    returns the pair ([sign_char], [abs_val]) where [sign_char] is ['-'] or
    ['+'], the sign of the signed value, and [abs_val] is its absolute value.
    [disp_lo] and [disp_hi] are expected to fit in one byte each.*)
let convert_disp_signed_16 disp_lo disp_hi =
    let disp_val = disp_lo + (disp_hi lsl 8) in
    if disp_val >= 0x8000 then ('-', 0x10000 - disp_val) else ('+', disp_val)

let disassemble_instr in_c =
    let a = input_byte in_c in
    match byte_to_opcode a with
    | Mov_reg_tofr_rm_opcode -> (
        print_string "mov ";
        let d_shift, d_mask = (1, 0x1) in
        let w_shift, w_mask = (0, 0x1) in
        let d, w = (field a d_shift d_mask, field a w_shift w_mask) in
        let mod_shift, mod_mask = (6, 0x03) in
        let reg_shift, reg_mask = (3, 0x07) in
        let rm_shift, rm_mask = (0, 0x07) in
        let b = input_byte in_c in
        let mod_val, reg_val, rm_val =
            ( field b mod_shift mod_mask,
              field b reg_shift reg_mask,
              field b rm_shift rm_mask )
        in
        let reg_name = reg_decode reg_val w in
        match byte_to_mod mod_val with
        | Mem_mod_8_disp ->
            let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
            if is_two_reg_rm_field rm_val then
              let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
              if d = 0 then
                Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
                  sign_char disp_lo reg_name
              else
                Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name
                  r2sum_name sign_char disp_lo
            else
              let rm_name = decode_eff_add_1 rm_val in
              if d = 0 then
                Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_lo
                  reg_name
              else
                Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char
                  disp_lo
        | Mem_mod_16_disp ->
            let disp_lo = input_byte in_c in
            let disp_hi = input_byte in_c in
            let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
            if is_two_reg_rm_field rm_val then
              let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
              if d = 0 then
                Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
                  sign_char disp_val reg_name
              else
                Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name
                  r2sum_name sign_char disp_val
            else
              let rm_name = decode_eff_add_1 rm_val in
              if d = 0 then
                Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_val
                  reg_name
              else
                Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char
                  disp_val
        | Mem_mod_no_disp ->
            if is_two_reg_rm_field rm_val then
              let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
              if d = 0 then
                Printf.printf "[%s + %s], %s\n" r1sum_name r2sum_name reg_name
              else
                Printf.printf "%s, [%s + %s]\n" reg_name r1sum_name r2sum_name
            else if not @@ is_dir_add_rm_field rm_val then
              let rm_name = decode_eff_add_1 rm_val in
              if d = 0 then Printf.printf "[%s], %s\n" rm_name reg_name
              else Printf.printf "%s, [%s]\n" reg_name rm_name
            else
              let disp_lo = input_byte in_c in
              let disp_hi = input_byte in_c in
              let sign_char, disp_val =
                  convert_disp_signed_16 disp_lo disp_hi
              in
              Printf.printf "%s, [%c%d]\n" reg_name sign_char disp_val
        | Reg_mod ->
            let rm_name = reg_decode rm_val w in
            Printf.printf "%s, %s\n"
              (if d = 0 then rm_name else reg_name)
              (if d = 0 then reg_name else rm_name)
        | Wrong_mod -> failwith "mod value does not exist")
    | Mov_imm_to_rm_opcode -> (
        print_string "mov ";
        let w_shift, w_mask = (0, 0x1) in
        let mod_shift, mod_mask = (6, 0x03) in
        let rm_shift, rm_mask = (0, 0x07) in
        let w = field a w_shift w_mask in
        let b = input_byte in_c in
        let mod_val, rm_val =
            (field b mod_shift mod_mask, field b rm_shift rm_mask)
        in
        let size_name = if w = 1 then "word" else "byte" in
        match byte_to_mod mod_val with
        | Mem_mod_8_disp ->
            let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
            let data_lo = input_byte in_c in
            let data_val =
                data_lo + if w = 1 then input_byte in_c lsl 8 else 0
            in
            if is_two_reg_rm_field rm_val then
              let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
              Printf.printf "[%s + %s %c %d], %s %d\n" r1sum_name r2sum_name
                sign_char disp_lo size_name data_val
            else
              let rm_name = decode_eff_add_1 rm_val in
              Printf.printf "[%s %c %d], %s %d\n" rm_name sign_char disp_lo
                size_name data_val
        | Mem_mod_16_disp ->
            let disp_lo = input_byte in_c in
            let disp_hi = input_byte in_c in
            let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
            let data_lo = input_byte in_c in
            let data_val =
                data_lo + if w = 1 then input_byte in_c lsl 8 else 0
            in
            if is_two_reg_rm_field rm_val then
              let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
              Printf.printf "[%s + %s %c %d], %s %d\n" r1sum_name r2sum_name
                sign_char disp_val size_name data_val
            else
              let rm_name = decode_eff_add_1 rm_val in
              Printf.printf "[%s %c %d], %s %d\n" rm_name sign_char disp_val
                size_name data_val
        | Mem_mod_no_disp ->
            if is_dir_add_rm_field rm_val then
              let disp_lo = input_byte in_c in
              let disp_hi = input_byte in_c in
              let sign_char, disp_val =
                  convert_disp_signed_16 disp_lo disp_hi
              in
              let data_lo = input_byte in_c in
              let data_val =
                  data_lo + if w = 1 then input_byte in_c lsl 8 else 0
              in
              Printf.printf "[%c%d] %s %d\n" sign_char disp_val size_name
                data_val
            else
              let data_lo = input_byte in_c in
              let data_val =
                  data_lo + if w = 1 then input_byte in_c lsl 8 else 0
              in
              if is_two_reg_rm_field rm_val then
                let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
                Printf.printf "[%s + %s], %s %d\n" r1sum_name r2sum_name
                  size_name data_val
              else
                let rm_name = decode_eff_add_1 rm_val in
                Printf.printf "[%s], %s %d\n" rm_name size_name data_val
        | Reg_mod ->
            let rm_name = reg_decode rm_val w in
            let data_lo = input_byte in_c in
            let data_val =
                data_lo + if w = 1 then input_byte in_c lsl 8 else 0
            in
            Printf.printf "%s, %d\n" rm_name data_val
        | Wrong_mod -> failwith "mod value does not exist")
    | Mov_imm_to_reg_opcode ->
        print_string "mov ";
        let w_shift, w_mask = (3, 0x1) in
        let reg_shift, reg_mask = (0, 0x07) in
        let w = field a w_shift w_mask in
        let data_lo = input_byte in_c in
        let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
        let reg_val = field a reg_shift reg_mask in
        let reg_name = reg_decode reg_val w in
        Printf.printf "%s, %d\n" reg_name data_val
    | Mov_mem_to_acc_opcode ->
        print_string "mov ";
        let addr_lo = input_byte in_c in
        let addr_val = addr_lo + (input_byte in_c lsl 8) in
        Printf.printf "ax, [%d]\n" addr_val
    | Mov_acc_to_mem_opcode ->
        print_string "mov ";
        let addr_lo = input_byte in_c in
        let addr_val = addr_lo + (input_byte in_c lsl 8) in
        Printf.printf "[%d], ax\n" addr_val
    | Wrong_opcode -> ()

let disassemble_file filename =
    print_endline "bits 16\n";
    let in_c = open_in_bin filename in
    try
      while true do
        disassemble_instr in_c
      done
    with End_of_file -> ()

let () =
    if Array.length Sys.argv <= 1 then
      failwith "No file name specified. Aborting."
    else disassemble_file Sys.argv.(1)
