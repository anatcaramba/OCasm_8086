open Opcodes

(***** Decoding utility functions and constants *****)

let mod_shift, mod_mask = (6, mask_r_2)
let rm_shift, rm_mask = (0, mask_r_3)
let seg_shift, seg_mask = (3, mask_r_2)
let flag_mask = mask_r_1
let reg_mask = mask_r_3
let d_shift = 1
let z_shift = 0
let v_shift = 1
let s_shift = 1

(** Contains the register represented by [reg_val] (or [rm_val] if [mod] is
    0b11), at position [w * 8 + reg_val] ([w] being 0 or 1). Has length 16.*)
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

(** [decode_reg reg_val w] returns the register name corresponding to [reg_val]
    and width [w].*)
let decode_reg reg_val w = reg_field_encoding.((w * 8) + reg_val)

(** Contains the segment register represented by [sr_val] at that position. Has
    length 4.*)
let seg_reg_field_encoding = [| "es"; "cs"; "ss"; "ds" |]

(** [decode_seg_reg seg_val] returns the name of the segment register prefix
    corresponding to [seg_val]. It therefore appends a colon [':'] if there
    indeed was a segment override. The prefix is empty is there was not.*)
let decode_seg_reg = function
    | None -> ""
    | Some seg_pref_val -> seg_reg_field_encoding.(seg_pref_val) ^ ":"

(** Concerns opcode [inc_or_dec_rm_opcode] : the table contains the instruction
    represented by [spec_val] at that position. Has length 8.*)
let instr_inc_or_dec_rm_encoding =
    [| "inc"; "dec"; "call"; "call far"; "jmp"; "jmp far"; "push"; "WRONG" |]

(** [decode_inc_or_dec_rm spec_val] returns [(spec_name, is_call_jump)] where
    [spec_name] is the right instruction name - of the kind inc/dec r/m, and
    [is_call_jump] is [true] iff the instruction is of the `jump` or `call`
    type.*)
let decode_inc_or_dec_rm spec_val =
    let spec_name = instr_inc_or_dec_rm_encoding.(spec_val) in
    ( spec_name,
      spec_name = "call" || spec_name = "jmp" || spec_name = "call far"
      || spec_name = "jmp far" )

(** Concerns opcode [arith_imm_tofr_rm_opcode] : the table contains the
    instruction represented by [spec_val] at that position. Has length 8.*)
let instr_arith_encoding =
    [| "add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp" |]

(** [decode_arith spec_val] returns the right instruction name - of the
    arithmetic kind. Can be used for all kinds of operations.*)
let decode_arith spec_val = instr_arith_encoding.(spec_val)

(** Concerns opcode [neg_mul_div_not_test_opcode] : the table contains the
    instruction represented by [spec_val] at that position. Has length 8.*)
let instr_neg_mul_etc_encoding =
    [| "test"; "WRONG"; "not"; "neg"; "mul"; "imul"; "div"; "idiv" |]

(** [decode_neg_mul spec_val] returns the right instruction name - of the
    arithmetic kind. Can be used for all kinds of operations.*)
let decode_neg_mul spec_val = instr_neg_mul_etc_encoding.(spec_val)

(** Concerns opcode [shift_rotate_opcode] : the table contains the instruction
    represented by [spec_val] at that position. Has length 8.*)
let instr_shift_rotate_encoding =
    [| "rol"; "ror"; "rcl"; "rcr"; "shl"; "shr"; "WRONG"; "sar" |]

(** [decode_shift_rotate spec_val] returns the right instruction name - of the
    shift/rotate kind. Can be used for all kinds of operations.*)
let decode_shift_rotate spec_val = instr_shift_rotate_encoding.(spec_val)

(** [are_equal_mask word target mask] returns true iff [word] matches [target]
    when applying [mask].*)
let are_equal_mask word target mask = word land mask = target land mask

(** [field word shift mask] returns the value obtained when shifting [word] to
    the right by [shift] bits, then applying [mask].*)
let field word shift mask = (word lsr shift) land mask

(** [byte_to_printing byte] takes in a byte and matches over the different
    available opcodes. It returns the correct [printing_t] corresponding to the
    required kind of assembly output.*)
let byte_to_printing byte =
    (* The search is linear in the number of opcodes. Some binary search on the
   sorted array might be possible, but because there are different masks, it
   also might not be simple.*)
    let n = Array.length opcodes_array in
    let rec linear_search i =
        if i = n then
          raise
          @@ Invalid_argument "Provided byte matches no valid instruction."
        else
          let { codebyte; mask; printing } = opcodes_array.(i) in
          if are_equal_mask codebyte byte mask then printing
          else linear_search (i + 1)
    in
    linear_search 0
(***** Aliases and utility functions for literal mod field values.*****)

(** Mod value for Memory mode with no displacement*)
let mod_mem_no_disp = 0X0

(** Mod value for Memory mode with 8-bit displacement*)
let mod_mem_8_disp = 0X1

(** Mod value for Memory mode with 16-bit displacement*)
let mod_mem_16_disp = 0X2

(** Mod value for Register mode (no displacement)*)
let mod_reg = 0X3

(** [is_two_reg_rm_field rm_val] returns [true] iff [rm_val] field represents a
    two register sum (with displacement or not).*)
let is_two_reg_rm_field rm_val = rm_val < 0X4

(** [is_dir_add_rm_field rm_val] returns [true] iff [rm_val] field represents a
    direct address calculation (in case MOD field indicates so).*)
let is_dir_add_rm_field rm_val = rm_val = 0X6

(** In the case of effective address calculation, if [rm_val] is the value of
    the RM field and represents the sum of two registers (with or without
    displacement), [decode_eff_add_2 rm_val] returns both registers' names.
    Raises [Invalid_argument] if [rm_val] is not a valid two-register R/M value.*)
let decode_eff_add_2 rm_val =
    if rm_val = 0X00 then ("bx", "si")
    else if rm_val = 0X01 then ("bx", "di")
    else if rm_val = 0X02 then ("bp", "si")
    else if rm_val = 0X03 then ("bp", "di")
    else
      raise
      @@ Invalid_argument
           "rm_val does not represent a two-register effective address \
            calculation"

(** In the case of effective address calculation, if [rm_val] is the value of
    the RM field and represents the sum of one register, and a potential
    displacement, [decode_eff_add_1 rm_val] returns the register's name. This
    does not take the special case of direct address calculation into account.
    Raises [Invalid_argument] if [rm_val] is not a valid one-register R/M value.*)
let decode_eff_add_1 rm_val =
    if rm_val = 0X04 then "si"
    else if rm_val = 0X05 then "di"
    else if rm_val = 0X06 then "bp"
    else if rm_val = 0X07 then "bx"
    else
      raise
      @@ Invalid_argument
           "rm_val does not represent a one-register effective address \
            calculation"

(** [convert_disp_signed_8 disp_lo] takes unsigned byte value [disp_lo] and
    converts it into the signed 2-complement value represented. It then returns
    the pair ([sign_char], [abs_val]) where [sign_char] is ['-'] or ['+'], the
    sign of the signed value, and [abs_val] is its absolute value. [disp_lo] is
    expected to fit in a byte.*)
let convert_disp_signed_8 disp_lo =
    if disp_lo >= 0X80 then ('-', 0x100 - disp_lo) else ('+', disp_lo)

(** [convert_disp_signed_16 disp_lo disp_hi] computes the value represented by
    the two displacement bytes in signed 2-complement representation. It then
    returns the pair ([sign_char], [abs_val]) where [sign_char] is ['-'] or
    ['+'], the sign of the signed value, and [abs_val] is its absolute value.
    [disp_lo] and [disp_hi] are expected to fit in one byte each.*)
let convert_disp_signed_16 disp_lo disp_hi =
    let disp_val = disp_lo + (disp_hi lsl 8) in
    if disp_val >= 0X8000 then ('-', 0x10000 - disp_val) else ('+', disp_val)

(** [convert_disp_unsigned_16 disp_lo disp_hi] returns the value represented by
    the two displacement bytes in unsigned representation. [disp_lo] and
    [disp_hi] are expected to fit in one byte each.*)
let convert_disp_unsigned_16 disp_lo disp_hi = disp_lo + (disp_hi lsl 8)

(** [compute_rm_string w mod_val in_c rm_val seg_pref_name] returns the string
    corresponding to the required display, being a register name or a memory
    operand (with or without displacement). It is prefixed by a segment if a
    prefix was provided by way of [seg_pref_name]. Other provided arguments are
    the [w] flag, the input channel [in_c] (required to read potential
    displacement), and field values [mod_val] and [rm_val].*)
let compute_rm_string w mod_val in_c rm_val seg_pref_name =
    if mod_val = mod_mem_8_disp then
      let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
      if is_two_reg_rm_field rm_val then
        let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
        Printf.sprintf "%s[%s+%s%c0x%x]" seg_pref_name r1sum_name r2sum_name
          sign_char disp_lo
      else
        let rm_name = decode_eff_add_1 rm_val in
        Printf.sprintf "%s[%s%c0x%x]" seg_pref_name rm_name sign_char disp_lo
    else if mod_val = mod_mem_16_disp then
      let disp_lo = input_byte in_c in
      let disp_hi = input_byte in_c in
      let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
      if is_two_reg_rm_field rm_val then
        let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
        Printf.sprintf "%s[%s+%s%c0x%x]" seg_pref_name r1sum_name r2sum_name
          sign_char disp_val
      else
        let rm_name = decode_eff_add_1 rm_val in
        Printf.sprintf "%s[%s%c0x%x]" seg_pref_name rm_name sign_char disp_val
    else if mod_val = mod_mem_no_disp then
      if is_two_reg_rm_field rm_val then
        let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
        Printf.sprintf "%s[%s+%s]" seg_pref_name r1sum_name r2sum_name
      else if not @@ is_dir_add_rm_field rm_val then
        let rm_name = decode_eff_add_1 rm_val in
        Printf.sprintf "%s[%s]" seg_pref_name rm_name
      else
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
        Printf.sprintf "%s[0x%x]" seg_pref_name disp_val
    else decode_reg rm_val w

(**[print_d_field_operand d reg_name rm_string] outputs the correct (partial)
   instruction in case of an operation using [d] flag. Provided are the
   [reg_name] (corresponding to the reg field) and [rm_string] (computed by
   [compute_rm_string], corresponding to the rm field).*)
let print_d_field_operand d reg_name rm_string =
    if d = 0 then Printf.printf "%s,%s\n" rm_string reg_name
    else Printf.printf "%s,%s\n" reg_name rm_string

(**[get_data_val w s in_c] reads from [in_c] the provided immediate value and
   returns it. [w] and/or [s] may be put to [0] if the operand does not use
   these flags.*)
let get_data_val w s in_c =
    let data_lo = input_byte in_c in
    data_lo
    +
    if w = 1 then
      if s = 0 then input_byte in_c lsl 8
      else if s = 1 && data_lo < 0 then 0XFF lsl 8
      else 0
    else 0

(**[get_mod_rm_val byte] fetches the mod and rm field values, provided the byte
   containing them. Returns [(mod_val, rm_val)]*)
let get_mod_rm_val byte =
    (field byte mod_shift mod_mask, field byte rm_shift rm_mask)

(**[print_instruction in_c byte seg_pref_val printing] takes in the input
   channel [in_c] from which binary instructions are read. [byte] is the first
   byte of the instruction, from which additional fields may be read. [printing]
   is the kind of printing required by given opcode. Also provided is
   [seg_pref_val], indicating if a segment prefix has been provided. It outputs
   the correct instruction to standard output, and returns the value of the
   segment prefix for the next instruction (in the form of a [int option]).*)
let rec print_instruction in_c byte seg_pref_val printing =
    let seg_pref_name = decode_seg_reg seg_pref_val in
    match printing with
    | Single instr_name ->
        print_string instr_name;
        None
    | Segment -> Some (field byte seg_shift seg_mask)
    | Escape ->
        print_endline "; esc (not implemented)";
        let byte2 = input_byte in_c in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        if mod_val = mod_mem_8_disp then ignore @@ input_byte in_c
        else if
          mod_val = mod_mem_16_disp
          || mod_val = mod_mem_no_disp
             && (not @@ is_two_reg_rm_field rm_val)
             && is_dir_add_rm_field rm_val
        then (
          ignore (input_byte in_c);
          ignore (input_byte in_c));
        None
    | Relative_disp instr_name ->
        let ip_lo = input_byte in_c in
        let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
        Printf.printf "%s $%c0x%x\n" instr_name sign_char inc_val;
        None
    | Integer_value ->
        let data_val = get_data_val 0 0 in_c in
        Printf.printf "int 0x%x\n" data_val;
        None
    | Direct_inter instr_name ->
        let ip_lo = input_byte in_c in
        let ip_hi = input_byte in_c in
        let ip_val = ip_lo + (ip_hi lsl 8) in
        let cs_lo = input_byte in_c in
        let cs_hi = input_byte in_c in
        let cs_val = cs_lo + (cs_hi lsl 8) in
        Printf.printf "%s 0x%x:0x%x\n" instr_name cs_val ip_val;
        None
    | Direct_in instr_name ->
        let inc_lo = input_byte in_c in
        let inc_hi = input_byte in_c in
        let sign_char, inc_val = convert_disp_signed_16 (inc_lo + 3) inc_hi in
        Printf.printf "%s $%c0x%x\n" instr_name sign_char inc_val;
        None
    | Jmp_direct_short ->
        let inc_lo = input_byte in_c in
        let sign_char, inc_val = convert_disp_signed_8 (inc_lo + 2) in
        Printf.printf "jmp $%c0x%x\n" sign_char inc_val;
        None
    | Single_width instr_name ->
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        Printf.printf "%s%c\n" instr_name (if w = 1 then 'w' else 'b');
        None
    | Ret_imm_sp ->
        let data_lo = input_byte in_c in
        let sign_char, data_val =
            convert_disp_signed_16 data_lo (input_byte in_c)
        in
        Printf.printf "ret %c0x%x\n" sign_char data_val;
        None
    | Rep ->
        let z = field byte z_shift flag_mask in
        Printf.printf "%s " (if z = 1 then "rep" else "repnz");
        None
    | Test_imm_acc ->
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        let data_val = get_data_val w 0 in_c in
        Printf.printf "test %s,0x%x\n" acc_name data_val;
        None
    | Test_xchg_rm_reg instr_name ->
        let byte2 = input_byte in_c in
        let reg_shift = 3 in
        let reg_val = field byte2 reg_shift reg_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        let reg_name = decode_reg reg_val w in
        Printf.printf "%s %s,%s\n" instr_name rm_string reg_name;
        None
    | Rm_reg instr_name ->
        Printf.printf "%s " instr_name;
        let w_shift = 0 in
        let d, w =
            (field byte d_shift flag_mask, field byte w_shift flag_mask)
        in
        let byte2 = input_byte in_c in
        let reg_shift = 3 in
        let reg_val = field byte2 reg_shift reg_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let reg_name = decode_reg reg_val w in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        print_d_field_operand d reg_name rm_string;
        None
    | Lea_les_lds instr_name ->
        let byte2 = input_byte in_c in
        let reg_shift = 3 in
        let reg_val = field byte2 reg_shift reg_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let reg_name = decode_reg reg_val 1 in
        let rm_string = compute_rm_string 1 mod_val in_c rm_val seg_pref_name in
        Printf.printf "%s %s,%s\n" instr_name reg_name rm_string;
        None
    | Arith_rm_reg ->
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte spec_shift spec_mask in
        let instr_name = decode_arith spec_val in
        print_instruction in_c byte seg_pref_val (Rm_reg instr_name)
    | Shift_rotate ->
        let byte2 = input_byte in_c in
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte2 spec_shift spec_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let v = field byte v_shift flag_mask in
        let instr_name = decode_shift_rotate spec_val in
        let size_name =
            if mod_val = mod_reg then "" else if w = 1 then "word " else "byte "
        in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        let v_val = if v = 1 then "cl" else "1" in
        Printf.printf "%s %s%s,%s\n" instr_name size_name rm_string v_val;
        None
    | Neg_mul_div_not_test ->
        let byte2 = input_byte in_c in
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte2 spec_shift spec_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let instr_name = decode_neg_mul spec_val in
        let size_name =
            if mod_val = mod_reg then "" else if w = 1 then "word " else "byte "
        in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        Printf.printf "%s %s%s" instr_name size_name rm_string;
        (if instr_name = "test" then
           let data_val = get_data_val w 0 in_c in
           Printf.printf ",0x%x" data_val);
        print_newline ();
        None
    | Inc_dec_push_pop_reg instr_name ->
        let reg_shift = 0 in
        let reg_val = field byte reg_shift reg_mask in
        let reg_name = decode_reg reg_val 1 in
        Printf.printf "%s %s\n" instr_name reg_name;
        None
    | Arith_imm_rm ->
        let byte2 = input_byte in_c in
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte2 spec_shift spec_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let s = field byte s_shift flag_mask in
        let instr_name = decode_arith spec_val in
        let size_name =
            if mod_val = mod_reg then "" else if w = 1 then "word " else "byte "
        in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        Printf.printf "%s %s%s" instr_name size_name rm_string;
        let data_val = get_data_val w s in_c in
        Printf.printf ",0x%x\n" data_val;
        None
    | Arith_imm_acc ->
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte spec_shift spec_mask in
        let instr_name = decode_arith spec_val in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        let data_val = get_data_val w 0 in_c in
        Printf.printf "%s %s,0x%x\n" instr_name acc_name data_val;
        None
    | Out_to_var ->
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        Printf.printf "out dx,%s\n" acc_name;
        None
    | Out_to_fixed ->
        let data_val = get_data_val 0 0 in_c in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        Printf.printf "out 0x%x,%s\n" data_val acc_name;
        None
    | In_from_var ->
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        Printf.printf "in %s,dx\n" acc_name;
        None
    | In_from_fixed ->
        let data_val = get_data_val 0 0 in_c in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let acc_name = if w = 1 then "ax" else "al" in
        Printf.printf "in %s,0x%x\n" acc_name data_val;
        None
    | Xchg_reg_acc ->
        let reg_shift = 0 in
        let reg_val = field byte reg_shift reg_mask in
        let reg_name = decode_reg reg_val 1 in
        if reg_name = "ax" then print_endline "nop"
        else Printf.printf "xchg ax,%s\n" reg_name;
        None
    | Push_pop_rm instr_name ->
        let byte2 = input_byte in_c in
        let mod_val, rm_val =
            (field byte2 mod_shift mod_mask, field byte2 rm_shift rm_mask)
        in
        let rm_string = compute_rm_string 1 mod_val in_c rm_val seg_pref_name in
        Printf.printf "%s word %s\n" instr_name rm_string;
        None
    | Inc_dec_rm ->
        let byte2 = input_byte in_c in
        let spec_shift, spec_mask = (3, mask_r_3) in
        let spec_val = field byte2 spec_shift spec_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let size_name = if w = 1 then "word " else "byte " in
        let instr_name, is_call_jump = decode_inc_or_dec_rm spec_val in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        Printf.printf "%s %s%s\n" instr_name
          (if is_call_jump || mod_val = mod_reg then "" else size_name)
          rm_string;
        None
    | Mov_mem_to_acc ->
        let addr_lo = input_byte in_c in
        let addr_val = addr_lo + (input_byte in_c lsl 8) in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        Printf.printf "mov a%c,%s[0x%x]\n"
          (if w = 1 then 'x' else 'l')
          seg_pref_name addr_val;
        None
    | Mov_acc_to_mem ->
        let addr_lo = input_byte in_c in
        let addr_val = addr_lo + (input_byte in_c lsl 8) in
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        Printf.printf "mov %s" seg_pref_name;
        if w = 1 then Printf.printf "[0x%x],ax\n" addr_val
        else Printf.printf "[0x%x],al\n" addr_val;
        None
    | Mov_imm_reg ->
        let w_shift = 3 in
        let reg_shift, reg_mask = (0, mask_r_3) in
        let w = field byte w_shift flag_mask in
        let data_val = get_data_val w 0 in_c in
        let reg_val = field byte reg_shift reg_mask in
        let reg_name = decode_reg reg_val w in
        Printf.printf "mov %s,0x%x\n" reg_name data_val;
        None
    | Mov_imm_to_rm ->
        let w_shift = 0 in
        let w = field byte w_shift flag_mask in
        let byte2 = input_byte in_c in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let size_name =
            if mod_val = mod_reg then "" else if w = 1 then "word " else "byte "
        in
        let rm_string = compute_rm_string w mod_val in_c rm_val seg_pref_name in
        let data_val = get_data_val w 0 in_c in
        Printf.printf "mov %s%s,0x%x\n" size_name rm_string data_val;
        None
    | Push_pop_seg_reg instr_name ->
        let reg_shift = 3 in
        let reg_val = field byte reg_shift reg_mask in
        (* Here we don't use helper function [decode_seg_reg], 
           which would add ':' after the segment register name*)
        let reg_name = seg_reg_field_encoding.(reg_val) in
        Printf.printf "%s %s\n" instr_name reg_name;
        None
    | Aam_aad instr_name ->
        let byte2 = input_byte in_c in
        if byte2 = 0x0A then Printf.printf "%s\n" instr_name
        else print_newline ();
        None
    | Mov_rm_to_seg_reg ->
        let byte2 = input_byte in_c in
        let seg_val = field byte2 seg_shift seg_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        (* Here we don't use helper function [decode_seg_reg], 
           which would add ':' after the segment register name*)
        let seg_reg_dest_name = seg_reg_field_encoding.(seg_val) in
        let rm_string = compute_rm_string 1 mod_val in_c rm_val seg_pref_name in
        Printf.printf "mov %s,%s" seg_reg_dest_name rm_string;
        None
    | Mov_seg_reg_to_rm ->
        let byte2 = input_byte in_c in
        let seg_val = field byte2 seg_shift seg_mask in
        let mod_val, rm_val = get_mod_rm_val byte2 in
        let seg_reg_src_name = seg_reg_field_encoding.(seg_val) in
        let rm_string = compute_rm_string 1 mod_val in_c rm_val seg_pref_name in
        Printf.printf "mov %s,%s\n" rm_string seg_reg_src_name;
        None

(**[disassemble_instr in_c seg_pref_val] takes in the input channel [in_c] from
   which binary instructions are read, along with an optional segment prefix
   value [seg_pref_val]. It reads the correct number of bytes from the channel
   and prints the corresponding assembly instruction to standard output.*)
let disassemble_instr in_c seg_pref_val =
    let byte = input_byte in_c in
    print_instruction in_c byte seg_pref_val (byte_to_printing byte)

(**[disassemble_file filename] opens file [filename] in binary mode to read all
   its bytes, and outputs the corresponding X8086 assembly instructions to
   standard output.*)
let disassemble_file filename =
    print_endline "bits 16\n";
    let in_c = open_in_bin filename in
    let rec loop seg_pref_val =
        try
          let seg_pref_val = disassemble_instr in_c seg_pref_val in
          loop seg_pref_val
        with End_of_file -> close_in in_c
    in
    loop None
