(***** Aliases for literal opcode byte values.
   The byte contains 0 where it is not specified for given opcode.*****)

(** Opcode value for "mov register/memory to/from register".*)
let mov_reg_tofr_rm_opcode = 0X88

(** Opcode value for "mov immediate to register/memory".*)
let mov_imm_to_reg_opcode = 0XB0

(** Opcode value for "mov immediate to register".*)
let mov_imm_to_rm_opcode = 0XC6

(** Opcode value for "mov memory to accumulator".*)
let mov_mem_to_acc_opcode = 0XA0

(** Opcode value for "mov accumulator to memory".*)
let mov_acc_to_mem_opcode = 0XA2

(** Opcode value for "mov register/memory to segment register".*)
let mov_rm_to_seg_reg_opcode = 0X8E

(** Opcode value for "mov segment register to register/memory".*)
let mov_seg_reg_to_rm_opcode = 0X8C

(**Opcode value for "push register".*)
let push_reg_opcode = 0X50

(**Opcode value for "push segment register".*)
let push_seg_reg_opcode = 0X06

(**Opcode value for "push register/memory".*)
let pop_rm_opcode = 0X8F

(**Opcode value for "pop register".*)
let pop_reg_opcode = 0X58

(**Opcode value for "pop segment register".*)
let pop_seg_reg_opcode = 0X07

(**Opcode value for "xchg register/memory with register".*)
let xchg_rm_reg_opcode = 0X86

(**Opcode value for "xchg register with accumulator".*)
let xchg_reg_acc_opcode = 0X90

(**Opcode value for "in from fixed port".*)
let in_from_fixed_opcode = 0XE4

(**Opcode value for "in from variable port".*)
let in_from_variable_opcode = 0XEC

(**Opcode value for "out to fixed port".*)
let out_to_fixed_opcode = 0XE6

(**Opcode value for "out to variable port".*)
let out_to_variable_opcode = 0XEE

(**Opcode value for "xlat".*)
let xlat_opcode = 0XD7

(**Opcode value for "lea".*)
let lea_opcode = 0X8D

(**Opcode value for "lds".*)
let lds_opcode = 0XC5

(**Opcode value for "les".*)
let les_opcode = 0XC4

(**Opcode value for "lahf".*)
let lahf_opcode = 0X9F

(**Opcode value for "sahf".*)
let sahf_opcode = 0X9E

(**Opcode value for "pushf".*)
let pushf_opcode = 0X9C

(**Opcode value for "popf".*)
let popf_opcode = 0X9D

(**Opcode value for any arithmeric operation "immediate to/fr register/memory".
   Knowing the specific operation requires looking at the following byte.
   Operations are "add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp" .*)
let arith_imm_tofr_rm_opcode = 0X80

(**Opcode value for any arithmeric operation "reg/memory and register to
   either". Knowing the specific operation requires looking at centrl bits.
   Operations are "add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp" .*)
let arith_rm_with_reg_opcode = 0X00

(**Opcode value for any arithmeric operation "immediate to accumulator". Knowing
   the specific operation requires looking at central bits. Operations are
   "add"; "or"; "adc"; "sbb"; "and"; "sub"; "xor"; "cmp" .*)
let arith_imm_to_acc_opcode = 0X04

(**Opcode value for several operations. Knowing which requires looking at the
   last bit, and the following byte. Possible operations are "inc
   register/memory", "dec register/memory", "push register/memory", "call
   indirect within segment", "call indirect intersegment", "jmp indirect within
   segment", "jmp indirect intersegment".*)
let inc_or_dec_rm_opcode = 0XFE

(**Opcode value for "inc register".*)
let inc_reg_opcode = 0X40

(**Opcode value for "aaa".*)
let aaa_opcode = 0X37

(**Opcode value for "daa".*)
let daa_opcode = 0X27

(**Opcode value for "dec register".*)
let dec_reg_opcode = 0X48

(**Opcode value for several logical/arithmetical operations. Knowing which
   requires looking at the following byte. Possible operations are "neg", "mul",
   "imul", "div", "idiv", "not", "test immediate data and register/memory".*)
let neg_mul_div_not_test_opcode = 0XF6

(**Opcode value for "aas".*)
let aas_opcode = 0X3F

(**Opcode value for "das".*)
let das_opcode = 0X2F

(**Opcode value for "aam".*)
let aam_opcode = 0XD4

(**Opcode value for "aad".*)
let aad_opcode = 0XD5

(**Opcode value for "cbw".*)
let cbw_opcode = 0X98

(**Opcode value for "cwd".*)
let cwd_opcode = 0X99

(**Opcode value for all "shift" and "rotate" operations.*)
let shift_rotate_opcode = 0XD0

(**Opcode value for "test register/memory and register".*)
let test_rm_and_reg_opcode = 0X85

(**Opcode value for "test immediate and accumulator".*)
let test_imm_and_acc_opcode = 0XA8

(**Opcode value for "rep".*)
let rep_opcode = 0XF2

(**Opcode value for "movs".*)
let movs_opcode = 0XA4

(**Opcode value for "cmps".*)
let cmps_opcode = 0XA6

(**Opcode value for "scas".*)
let scas_opcode = 0XAE

(**Opcode value for "lods".*)
let lods_opcode = 0XAC

(**Opcode value for "stos".*)
let stos_opcode = 0XAA

(**Opcode value for "call direct within segment".*)
let call_direct_in_opcode = 0XE8

(**Opcode value for "call direct intersegment".*)
let call_direct_inter_opcode = 0X9A

(**Opcode value for "jmp direct within segment".*)
let jmp_direct_in_opcode = 0XE9

(**Opcode value for "jmp direct within segment-short".*)
let jmp_direct_short_opcode = 0XEB

(**Opcode value for "jmp direct intersegment".*)
let jmp_direct_inter_opcode = 0XEA

(**Opcode value for "ret within segment".*)
let ret_within_opcode = 0XC3

(**Opcode value for "ret within segment adding immed to sp".*)
let ret_within_imm_sp_opcode = 0XC2

(**Opcode value for "ret intersegment".*)
let ret_inter_opcode = 0XCB

(**Opcode value for "ret intersegment adding immed to sp".*)
let ret_inter_imm_sp_opcode = 0XCA

(**Opcode value for "jmp on equal/zero".*)
let jz_opcode = 0X74

(**Opcode value for "jmp on less/not greater or equal".*)
let jnge_opcode = 0X7C

(**Opcode value for "jmp on less or equal/not greater".*)
let jle_opcode = 0X7E

(**Opcode value for "jmp on below/not above or equal".*)
let jnae_opcode = 0X72

(**Opcode value for "jmp on below or equal".*)
let jbe_opcode = 0X76

(**Opcode value for "jmp on parity even".*)
let jpe_opcode = 0X7A

(**Opcode value for "jmp on overflow".*)
let jo_opcode = 0X70

(**Opcode value for "jmp on sign".*)
let js_opcode = 0X78

(**Opcode value for "jmp on not zero/not equal".*)
let jnz_opcode = 0X75

(**Opcode value for "jmp on not less/greater or equal".*)
let jge_opcode = 0X7D

(**Opcode value for "jmp on not less or equal/greater".*)
let jnle_opode = 0X7F

(**Opcode value for "jmp on not below/ above or equal".*)
let jae_opcode = 0X73

(**Opcode value for "jmp on not below or equal/ above".*)
let jnbe_opcode = 0X77

(**Opcode value for "jmp on parity odd".*)
let jnp_opcode = 0X7B

(**Opcode value for "jmp on not overflow".*)
let jno_opcode = 0X71

(**Opcode value for "jmp on not sign".*)
let jns_opcode = 0X79

(**Opcode value for "loop".*)
let loop_opcode = 0XE2

(**Opcode value for "loop while zero/equal".*)
let loopz_opcode = 0XE1

(**Opcode value for "loop while not zero/not equal".*)
let loopnz_opcode = 0XE0

(**Opcode value for "jmp on cx zero".*)
let jcxz_opcode = 0XE3

(**Opcode value for "int type specified".*)
let int_type_spec_opcode = 0XCD

(**Opcode value for "int type 3".*)
let int_type_3_opcode = 0XCC

(**Opcode value for "into".*)
let into_opcode = 0XCE

(**Opcode value for "iret".*)
let iret_opcode = 0XCF

(**Opcode value for "clc".*)
let clc_opcode = 0XF8

(**Opcode value for "cmc".*)
let cmc_opcode = 0XF5

(**Opcode value for "stc".*)
let stc_opcode = 0XF9

(**Opcode value for "cld".*)
let cld_opcode = 0XFC

(**Opcode value for "std".*)
let std_opcode = 0XFD

(**Opcode value for "cli".*)
let cli_opcode = 0XFA

(**Opcode value for "sti".*)
let sti_opcode = 0XFB

(**Opcode value for "hlt".*)
let hlt_opcode = 0XF4

(**Opcode value for "wait".*)
let wait_opcode = 0X9B

(**Opcode value for "esc".*)
let esc_opcode = 0XD8

(**Opcode value for "lock".*)
let lock_opcode = 0XF0

(**Opcode value for "segment".*)
let segment_opcode = 0X26

(***** Decoding utility functions and constants *****)

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

(** [decode_reg reg_val w] returns the register name corresponding to [reg_val]
    and width [w].*)
let decode_reg reg_val w = reg_field_encoding.((w * 8) + reg_val)

(** Contains the segment register represented by [sr_val] at that position. Has
    length 4.*)
let seg_reg_field_encoding = [| "es"; "cs"; "ss"; "ds" |]

(** [decode_seg_reg seg_val] returns the name of the segment register
    corresponding to [seg_val].*)
let decode_seg_reg seg_val = seg_reg_field_encoding.(seg_val)

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

(** [are_equal_mask word taret mask] returns true iff [word] matches [target]
    when applying [mask].*)
let are_equal_mask word target mask = word land mask = target land mask

(** [field word shift mask] returns the value obtained when shifting [word] to
    the right by [shift] bits, then applying [mask].*)
let field word shift mask = (word lsr shift) land mask

(** Mask byte values such that [mask_l_k] checks is mask consisting of [k] 1s to
    the left, then 0s.*)
let mask_l_8, mask_l_7, mask_l_6, mask_l_5, mask_l_4, mask_l_3 =
    (0XFF, 0XFE, 0XFC, 0XF8, 0XF0, 0xE0)

(** Mask byte values such that [mask_r_k] checks is mask consisting of [k] 1s to
    the right, then 0s.*)
let mask_r_3, mask_r_2, mask_r_1 = (0X07, 0X03, 0X01)

(***** Aliases for literal mod field values.*****)

(** Mod value for Memory mode with no displacement*)
let mem_mod_no_disp = 0X0

(** Mod value for Memory mode with 8-bit displacement*)
let mem_mod_8_disp = 0X1

(** Mod value for Memory mode with 16-bit displacement*)
let mem_mod_16_disp = 0X2

(** Mod value for Register mode (no displacement)*)
let reg_mod = 0X3

(** [is_two_reg_rm_field rm_val] returns [true] iff [rm_val] field represents a
    two register sum (with displacement or not).*)
let is_two_reg_rm_field rm_val = rm_val < 0X4

(** [is_dir_add_rm_field rm_val] returns [true] iff [rm_val] field represents a
    direct address calculation (in case MOD field indicates so).*)
let is_dir_add_rm_field rm_val = rm_val = 0X6

(** In the case of effective address calculation, if [rm_val] is the value of
    the RM field and represents the sum of two registers (with or without
    displacement), [decode_eff_add_2 rm_val] returns both registers' names.
    Raises {!Invalid_argument} if [rm_val] is not a valid two-register R/M
    value.*)
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
    Raises {!Invalid_argument} if [rm_val] is not a valid one-register R/M
    value.*)
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

(** [convert_disp_signed_8 disp_lo] changes unsigned byte value [disp_lo] and
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

(** [disassemble_instr] takes in an [in_channel] and reads as many bytes from it
    as needed, to decode one 80x86 assembly instruction. It prints out the
    instruction to standard output.*)
let disassemble_instr in_c seg_prefix_val =
    let a = input_byte in_c in
    let seg_pref_string =
        if !seg_prefix_val >= 0 then (
          let seg_name = decode_seg_reg !seg_prefix_val in
          seg_prefix_val := -1;
          seg_name ^ ":")
        else ""
    in
    (* Printf.printf "!!!!!!!!!!!!  %X  !!!!!!!!!!!!!!!\n" a; *)
    if are_equal_mask a mov_reg_tofr_rm_opcode mask_l_6 then (
      print_string "mov ";
      let d_shift, d_mask = (1, mask_r_1) in
      let w_shift, w_mask = (0, mask_r_1) in
      let d, w = (field a d_shift d_mask, field a w_shift w_mask) in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let reg_shift, reg_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let b = input_byte in_c in
      let mod_val, reg_val, rm_val =
          ( field b mod_shift mod_mask,
            field b reg_shift reg_mask,
            field b rm_shift rm_mask )
      in
      let reg_name = decode_reg reg_val w in

      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "%s[%s + %s %c %d], %s\n" seg_pref_string r1sum_name
              r2sum_name sign_char disp_lo reg_name
          else
            Printf.printf "%s, %s[%s + %s %c %d]\n" reg_name seg_pref_string
              r1sum_name r2sum_name sign_char disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "%s[%s %c %d], %s\n" seg_pref_string rm_name sign_char
              disp_lo reg_name
          else
            Printf.printf "%s, %s[%s %c %d]\n" reg_name seg_pref_string rm_name
              sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "%s[%s + %s %c %d], %s\n" seg_pref_string r1sum_name
              r2sum_name sign_char disp_val reg_name
          else
            Printf.printf "%s, %s[%s + %s %c %d]\n" reg_name seg_pref_string
              r1sum_name r2sum_name sign_char disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "%s[%s %c %d], %s\n" seg_pref_string rm_name sign_char
              disp_val reg_name
          else
            Printf.printf "%s, %s[%s %c %d]\n" reg_name seg_pref_string rm_name
              sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "%s[%s + %s], %s\n" seg_pref_string r1sum_name
              r2sum_name reg_name
          else
            Printf.printf "%s, %s[%s + %s]\n" reg_name seg_pref_string
              r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "%s[%s], %s\n" seg_pref_string rm_name reg_name
          else Printf.printf "%s, %s[%s]\n" reg_name seg_pref_string rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          Printf.printf "%s, %s[%d]\n" reg_name seg_pref_string disp_val
      else
        let rm_name = decode_reg rm_val w in
        Printf.printf "%s, %s\n"
          (if d = 0 then rm_name else reg_name)
          (if d = 0 then reg_name else rm_name))
    else if are_equal_mask a mov_imm_to_rm_opcode mask_l_7 then (
      print_string "mov ";
      let mod_shift, mod_mask = (6, mask_r_2) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let b = input_byte in_c in
      let mod_val, rm_val =
          (field b mod_shift mod_mask, field b rm_shift rm_mask)
      in
      let size_name = if w = 1 then "word" else "byte" in
      print_string seg_pref_string;
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        let data_lo = input_byte in_c in
        let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d], %s %d\n" r1sum_name r2sum_name
            sign_char disp_lo size_name data_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d], %s %d\n" rm_name sign_char disp_lo
            size_name data_val
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        let data_lo = input_byte in_c in
        let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d], %s %d\n" r1sum_name r2sum_name
            sign_char disp_val size_name data_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d], %s %d\n" rm_name sign_char disp_val
            size_name data_val
      else if mod_val = mem_mod_no_disp then
        if is_dir_add_rm_field rm_val then
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          let data_lo = input_byte in_c in
          let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
          Printf.printf "[%d] %s %d\n" disp_val size_name data_val
        else
          let data_lo = input_byte in_c in
          let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
          if is_two_reg_rm_field rm_val then
            let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
            Printf.printf "[%s + %s], %s %d\n" r1sum_name r2sum_name size_name
              data_val
          else
            let rm_name = decode_eff_add_1 rm_val in
            Printf.printf "[%s], %s %d\n" rm_name size_name data_val
      else
        let rm_name = decode_reg rm_val w in
        let data_lo = input_byte in_c in
        let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
        Printf.printf "%s, %d\n" rm_name data_val)
    else if are_equal_mask a mov_imm_to_reg_opcode mask_l_4 then (
      print_string "mov ";
      let w_shift, w_mask = (3, mask_r_1) in
      let reg_shift, reg_mask = (0, mask_r_3) in
      let w = field a w_shift w_mask in
      let data_lo = input_byte in_c in
      let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val w in
      Printf.printf "%s, %d\n" reg_name data_val)
    else if are_equal_mask a mov_mem_to_acc_opcode mask_l_7 then (
      print_string "mov ";
      let addr_lo = input_byte in_c in
      let addr_val = addr_lo + (input_byte in_c lsl 8) in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "a%c, %s[%d]\n"
        (if w = 1 then 'x' else 'l')
        seg_pref_string addr_val)
    else if are_equal_mask a mov_acc_to_mem_opcode mask_l_7 then (
      print_string "mov ";
      print_string seg_pref_string;
      let addr_lo = input_byte in_c in
      let addr_val = addr_lo + (input_byte in_c lsl 8) in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      if w = 1 then Printf.printf "[%d], ax\n" addr_val
      else Printf.printf "[%d], al\n" addr_val)
    else if
      are_equal_mask a mov_rm_to_seg_reg_opcode mask_l_8
      || are_equal_mask a mov_seg_reg_to_rm_opcode mask_l_8
    then (
      print_string "mov ";
      let is_rm_to_seg = are_equal_mask a mov_rm_to_seg_reg_opcode mask_l_8 in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let sr_shift, sr_mask = (3, mask_r_2) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let b = input_byte in_c in
      let mod_val, sr_val, rm_val =
          ( field b mod_shift mod_mask,
            field b sr_shift sr_mask,
            field b rm_shift rm_mask )
      in
      let sr_name = seg_reg_field_encoding.(sr_val) in
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s + %s %c %d]\n" sr_name seg_pref_string
              r1sum_name r2sum_name sign_char disp_lo
          else
            Printf.printf "%s[%s + %s %c %d], %s\n" seg_pref_string r1sum_name
              r2sum_name sign_char disp_lo sr_name
        else
          let rm_name = decode_eff_add_1 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s %c %d]\n" sr_name seg_pref_string rm_name
              sign_char disp_lo
          else
            Printf.printf "%s[%s %c %d], %s\n" rm_name seg_pref_string sign_char
              disp_lo sr_name
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s + %s %c %d]\n" sr_name seg_pref_string
              r1sum_name r2sum_name sign_char disp_val
          else
            Printf.printf "%s[%s + %s %c %d], %s\n" seg_pref_string r1sum_name
              r2sum_name sign_char disp_val sr_name
        else
          let rm_name = decode_eff_add_1 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s %c %d]\n" sr_name seg_pref_string rm_name
              sign_char disp_val
          else
            Printf.printf "%s[%s %c %d], %s\n" seg_pref_string rm_name sign_char
              disp_val sr_name
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s + %s]\n" sr_name seg_pref_string r1sum_name
              r2sum_name
          else
            Printf.printf "%s[%s + %s], %s\n" seg_pref_string r1sum_name
              r2sum_name sr_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%s]\n" sr_name seg_pref_string rm_name
          else Printf.printf "%s[%s], %s\n" seg_pref_string rm_name sr_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          if is_rm_to_seg then
            Printf.printf "%s, %s[%d]\n" sr_name seg_pref_string disp_val
          else Printf.printf "%s[%d], %s\n" seg_pref_string disp_val sr_name
      else
        let rm_name = decode_reg rm_val 1 in
        if is_rm_to_seg then Printf.printf "%s, %s\n" sr_name rm_name
        else Printf.printf "%s, %s\n" rm_name sr_name)
    else if are_equal_mask a push_reg_opcode mask_l_5 then
      let reg_shift, reg_mask = (0, mask_r_3) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val 1 in
      Printf.printf "push %s\n" reg_name
    else if are_equal_mask a push_seg_reg_opcode (mask_l_3 lor mask_r_3) then
      let reg_shift, reg_mask = (3, mask_r_2) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = seg_reg_field_encoding.(reg_val) in
      Printf.printf "push %s\n" reg_name
    else if are_equal_mask a pop_rm_opcode mask_l_8 then (
      print_string "pop ";
      let mod_shift, mod_mask = (6, mask_r_2) in
      let rm_shift, rm_mask = (0, 0X07) in
      let b = input_byte in_c in
      let mod_val, rm_val =
          (field b mod_shift mod_mask, field b rm_shift rm_mask)
      in
      print_string ("word " ^ seg_pref_string);
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d]\n" r1sum_name r2sum_name sign_char
            disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d]\n" rm_name sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d]\n" r1sum_name r2sum_name sign_char
            disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d]\n" rm_name sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s]\n" r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s]\n" rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          Printf.printf "[%d]\n" disp_val
      else
        let rm_name = decode_reg rm_val 1 in
        Printf.printf "%s\n" rm_name)
    else if are_equal_mask a pop_reg_opcode mask_l_5 then
      let reg_shift, reg_mask = (0, mask_r_3) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val 1 in
      Printf.printf "pop %s\n" reg_name
    else if are_equal_mask a pop_seg_reg_opcode (mask_l_3 lor mask_r_3) then
      let reg_shift, reg_mask = (3, mask_r_2) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = seg_reg_field_encoding.(reg_val) in
      Printf.printf "pop %s\n" reg_name
    else if are_equal_mask a inc_or_dec_rm_opcode mask_l_7 then (
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let spec_shift, spec_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, spec_val, rm_val =
          ( field b mod_shift mod_mask,
            field b spec_shift spec_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let size_name = if w = 1 then "word " else "byte " in
      let instr_name, is_call_jump = decode_inc_or_dec_rm spec_val in
      Printf.printf "%s %s%s" instr_name
        (if is_call_jump || mod_val = reg_mod then "" else size_name)
        seg_pref_string;
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d]\n" r1sum_name r2sum_name sign_char
            disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d]\n" rm_name sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s %c %d]\n" r1sum_name r2sum_name sign_char
            disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s %c %d]\n" rm_name sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "[%s + %s]\n" r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "[%s]\n" rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          Printf.printf "[%d]\n" disp_val
      else
        let rm_name = decode_reg rm_val w in
        Printf.printf "%s\n" rm_name)
    else if are_equal_mask a xchg_rm_reg_opcode mask_l_7 then (
      print_string "xchg ";
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let reg_shift, reg_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, reg_val, rm_val =
          ( field b mod_shift mod_mask,
            field b reg_shift reg_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let reg_name = decode_reg reg_val w in
      (if mod_val = mem_mod_8_disp then
         let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "[%s + %s %c %d], " r1sum_name r2sum_name sign_char
             disp_lo
         else
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "[%s %c %d], " rm_name sign_char disp_lo
       else if mod_val = mem_mod_16_disp then
         let disp_lo = input_byte in_c in
         let disp_hi = input_byte in_c in
         let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "[%s + %s %c %d], " r1sum_name r2sum_name sign_char
             disp_val
         else
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "[%s %c %d], " rm_name sign_char disp_val
       else if mod_val = mem_mod_no_disp then
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "[%s + %s], " r1sum_name r2sum_name
         else if not @@ is_dir_add_rm_field rm_val then
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "[%s], " rm_name
         else
           let disp_lo = input_byte in_c in
           let disp_hi = input_byte in_c in
           let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
           Printf.printf "[%d], " disp_val
       else
         let rm_name = decode_reg rm_val w in
         Printf.printf "%s, " rm_name);
      print_endline reg_name)
    else if are_equal_mask a xchg_reg_acc_opcode mask_l_5 then (
      print_string "xchg ";
      let reg_shift, reg_mask = (0, mask_r_3) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val 1 in
      Printf.printf "ax, %s\n" reg_name)
    else if are_equal_mask a in_from_fixed_opcode mask_l_7 then
      let data_val = input_byte in_c in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      Printf.printf "in %s, %d\n" acc_name data_val
    else if are_equal_mask a in_from_variable_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      Printf.printf "in %s, dx\n" acc_name
    else if are_equal_mask a out_to_fixed_opcode mask_l_7 then
      let data_val = input_byte in_c in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      Printf.printf "out %d, %s\n" data_val acc_name
    else if are_equal_mask a out_to_variable_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      Printf.printf "out dx, %s\n" acc_name
    else if are_equal_mask a xlat_opcode mask_l_8 then print_endline "xlat"
    else if
      are_equal_mask a lea_opcode mask_l_8
      || are_equal_mask a lds_opcode mask_l_8
      || are_equal_mask a les_opcode mask_l_8
    then (
      let instr_name =
          if are_equal_mask a lea_opcode mask_l_8 then "lea "
          else if are_equal_mask a lds_opcode mask_l_8 then "lds "
          else "les "
      in
      print_string instr_name;
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let reg_shift, reg_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, reg_val, rm_val =
          ( field b mod_shift mod_mask,
            field b reg_shift reg_mask,
            field b rm_shift rm_mask )
      in
      let reg_name = decode_reg reg_val 1 in
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
            sign_char disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
            sign_char disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s, [%s + %s]\n" reg_name r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s, [%s]\n" reg_name rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          Printf.printf "%s, [%d]\n" reg_name disp_val
      else
        let rm_name = decode_reg rm_val 1 in
        Printf.printf "%s, %s\n" reg_name rm_name)
    else if are_equal_mask a lahf_opcode mask_l_8 then print_endline "lahf"
    else if are_equal_mask a sahf_opcode mask_l_8 then print_endline "sahf"
    else if are_equal_mask a pushf_opcode mask_l_8 then print_endline "pushf"
    else if are_equal_mask a popf_opcode mask_l_8 then print_endline "popf"
    else if are_equal_mask a arith_imm_tofr_rm_opcode mask_l_6 then (
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let spec_shift, spec_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, spec_val, rm_val =
          ( field b mod_shift mod_mask,
            field b spec_shift spec_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let s_shift, s_mask = (1, mask_r_1) in
      let s = field a s_shift s_mask in
      let instr_name = decode_arith spec_val in
      let size_name = if w = 1 then "word" else "byte" in
      Printf.printf "%s " instr_name;
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        let data_lo = input_byte in_c in
        let data_val =
            data_lo
            +
            if w = 1 then
              if s = 0 then input_byte in_c lsl 8
              else if s = 1 && data_lo < 0 then 0XFF lsl 8
              else 0
            else 0
        in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s [%s + %s %c %d], %d\n" size_name r1sum_name
            r2sum_name sign_char disp_lo data_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s [%s %c %d], %d\n" size_name rm_name sign_char
            disp_lo data_val
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        let data_lo = input_byte in_c in
        let data_val =
            data_lo
            +
            if w = 1 then
              if s = 0 then input_byte in_c lsl 8
              else if s = 1 && data_lo < 0 then 0XFF lsl 8
              else 0
            else 0
        in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s [%s + %s %c %d], %d\n" size_name r1sum_name
            r2sum_name sign_char disp_val data_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s [%s %c %d], %d\n" size_name rm_name sign_char
            disp_val data_val
      else if mod_val = mem_mod_no_disp then
        if is_dir_add_rm_field rm_val then
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          let data_lo = input_byte in_c in
          let data_val =
              data_lo
              +
              if w = 1 then
                if s = 0 then input_byte in_c lsl 8
                else if s = 1 && data_lo < 0 then 0XFF lsl 8
                else 0
              else 0
          in
          Printf.printf "%s [%d] %d\n" size_name disp_val data_val
        else
          let data_lo = input_byte in_c in
          let data_val =
              data_lo
              +
              if w = 1 then
                if s = 0 then input_byte in_c lsl 8
                else if s = 1 && data_lo < 0 then 0XFF lsl 8
                else 0
              else 0
          in
          if is_two_reg_rm_field rm_val then
            let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
            Printf.printf "%s [%s + %s], %d\n" size_name r1sum_name r2sum_name
              data_val
          else
            let rm_name = decode_eff_add_1 rm_val in
            Printf.printf "%s [%s], %d\n" size_name rm_name data_val
      else
        let rm_name = decode_reg rm_val w in
        let data_lo = input_byte in_c in
        let data_val =
            data_lo
            +
            if w = 1 then
              if s = 0 then input_byte in_c lsl 8
              else if s = 1 && data_lo < 0 then 0XFF lsl 8
              else 0
            else 0
        in
        Printf.printf "%s, %d\n" rm_name data_val)
    else if are_equal_mask arith_rm_with_reg_opcode a 0XC4 then (
      let spec_shift, spec_mask = (3, mask_r_3) in
      let spec_val = field a spec_shift spec_mask in
      let instr_name = decode_arith spec_val in
      Printf.printf "%s " instr_name;
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let reg_shift, reg_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, reg_val, rm_val =
          ( field b mod_shift mod_mask,
            field b reg_shift reg_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let d_shift, d_mask = (1, mask_r_1) in
      let d = field a d_shift d_mask in
      let reg_name = decode_reg reg_val w in
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
              sign_char disp_lo reg_name
          else
            Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
              sign_char disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_lo reg_name
          else
            Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
              sign_char disp_val reg_name
          else
            Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
              sign_char disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_val reg_name
          else
            Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s], %s\n" r1sum_name r2sum_name reg_name
          else Printf.printf "%s, [%s + %s]\n" reg_name r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then Printf.printf "[%s], %s\n" rm_name reg_name
          else Printf.printf "%s, [%s]\n" reg_name rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          if d = 0 then Printf.printf "[%d], %s\n" disp_val reg_name
          else Printf.printf "%s, [%d]\n" reg_name disp_val
      else
        let rm_name = decode_reg rm_val w in
        if d = 0 then Printf.printf "%s, %s\n" rm_name reg_name
        else Printf.printf "%s, %s\n" reg_name rm_name)
    else if are_equal_mask arith_imm_to_acc_opcode a 0XC6 then (
      let spec_shift, spec_mask = (3, mask_r_3) in
      let spec_val = field a spec_shift spec_mask in
      let instr_name = decode_arith spec_val in
      Printf.printf "%s " instr_name;
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      let data_lo = input_byte in_c in
      let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
      Printf.printf "%s, %d\n" acc_name data_val)
    else if are_equal_mask a inc_reg_opcode mask_l_5 then
      let reg_shift, reg_mask = (0, mask_r_3) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val 1 in
      Printf.printf "inc %s\n" reg_name
    else if are_equal_mask a aaa_opcode mask_l_8 then print_endline "aaa"
    else if are_equal_mask a daa_opcode mask_l_8 then print_endline "daa"
    else if are_equal_mask a dec_reg_opcode mask_l_5 then
      let reg_shift, reg_mask = (0, mask_r_3) in
      let reg_val = field a reg_shift reg_mask in
      let reg_name = decode_reg reg_val 1 in
      Printf.printf "dec %s\n" reg_name
    else if are_equal_mask a neg_mul_div_not_test_opcode mask_l_7 then (
      let b = input_byte in_c in
      (* Printf.printf "bbbbbbbbbbbbb   %X   bbbbbbbbbb\n" b; *)
      let mod_shift, mod_mask = (6, mask_r_2) in
      let spec_shift, spec_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, spec_val, rm_val =
          ( field b mod_shift mod_mask,
            field b spec_shift spec_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let instr_name = decode_neg_mul spec_val in
      let size_name = if w = 1 then "word" else "byte" in
      (if mod_val = mem_mod_8_disp then
         let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "%s %s [%s + %s %c %d]" instr_name size_name r1sum_name
             r2sum_name sign_char disp_lo
         else
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "%s %s [%s %c %d]" instr_name size_name rm_name
             sign_char disp_lo
       else if mod_val = mem_mod_16_disp then
         let disp_lo = input_byte in_c in
         let disp_hi = input_byte in_c in
         let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "%s %s [%s + %s %c %d]" instr_name size_name r1sum_name
             r2sum_name sign_char disp_val
         else
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "%s %s [%s %c %d]" instr_name size_name rm_name
             sign_char disp_val
       else if mod_val = mem_mod_no_disp then
         if is_two_reg_rm_field rm_val then
           let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
           Printf.printf "%s %s [%s + %s]" instr_name size_name r1sum_name
             r2sum_name
         else if not @@ is_dir_add_rm_field rm_val then
           let rm_name = decode_eff_add_1 rm_val in
           Printf.printf "%s %s [%s]" instr_name size_name rm_name
         else
           let disp_lo = input_byte in_c in
           let disp_hi = input_byte in_c in
           let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
           Printf.printf "%s %s [%d]" instr_name size_name disp_val
       else
         let rm_name = decode_reg rm_val w in
         Printf.printf "%s %s" instr_name rm_name);
      (if instr_name = "test" then
         let data_lo = input_byte in_c in
         let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
         Printf.printf ", %d" data_val);
      print_newline ())
    else if are_equal_mask a aas_opcode mask_l_8 then print_endline "aas"
    else if are_equal_mask a das_opcode mask_l_8 then print_endline "das"
    else if are_equal_mask a aam_opcode mask_l_8 then (
      print_string "aam";
      let b = input_byte in_c in
      if b <> 0x0A then Printf.printf " %d\n" b else print_newline ())
    else if are_equal_mask a aad_opcode mask_l_8 then (
      print_string "aad";
      let b = input_byte in_c in
      if b <> 0x0A then Printf.printf " %d\n" b else print_newline ())
    else if are_equal_mask a cbw_opcode mask_l_8 then print_endline "cbw"
    else if are_equal_mask a cwd_opcode mask_l_8 then print_endline "cwd"
    else if are_equal_mask a shift_rotate_opcode mask_l_6 then
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let spec_shift, spec_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, spec_val, rm_val =
          ( field b mod_shift mod_mask,
            field b spec_shift spec_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let v_shift, v_mask = (1, mask_r_1) in
      let v = field a v_shift v_mask in
      let instr_name = decode_shift_rotate spec_val in
      let size_name = if w = 1 then "word" else "byte" in
      let v_val = if v = 1 then "cl" else "1" in
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s %s [%s + %s %c %d], %s\n" instr_name size_name
            r1sum_name r2sum_name sign_char disp_lo v_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s %s [%s %c %d], %s\n" instr_name size_name rm_name
            sign_char disp_lo v_val
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s %s [%s + %s %c %d], %s\n" instr_name size_name
            r1sum_name r2sum_name sign_char disp_val v_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s %s [%s %c %d], %s\n" instr_name size_name rm_name
            sign_char disp_val v_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          Printf.printf "%s %s [%s + %s], %s\n" instr_name size_name r1sum_name
            r2sum_name v_val
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          Printf.printf "%s %s [%s], %s\n" instr_name size_name rm_name v_val
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          Printf.printf "%s %s [%d], %s\n" instr_name size_name disp_val v_val
      else
        let rm_name = decode_reg rm_val w in
        Printf.printf "%s %s, %s\n" instr_name rm_name v_val
    else if are_equal_mask a test_rm_and_reg_opcode mask_l_6 then (
      Printf.printf "test ";
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let reg_shift, reg_mask = (3, mask_r_3) in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let mod_val, reg_val, rm_val =
          ( field b mod_shift mod_mask,
            field b reg_shift reg_mask,
            field b rm_shift rm_mask )
      in
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let d_shift, d_mask = (1, mask_r_1) in
      let d = field a d_shift d_mask in
      let reg_name = decode_reg reg_val w in
      if mod_val = mem_mod_8_disp then
        let sign_char, disp_lo = convert_disp_signed_8 @@ input_byte in_c in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
              sign_char disp_lo reg_name
          else
            Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
              sign_char disp_lo
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_lo reg_name
          else
            Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_lo
      else if mod_val = mem_mod_16_disp then
        let disp_lo = input_byte in_c in
        let disp_hi = input_byte in_c in
        let sign_char, disp_val = convert_disp_signed_16 disp_lo disp_hi in
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s %c %d], %s\n" r1sum_name r2sum_name
              sign_char disp_val reg_name
          else
            Printf.printf "%s, [%s + %s %c %d]\n" reg_name r1sum_name r2sum_name
              sign_char disp_val
        else
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then
            Printf.printf "[%s %c %d], %s\n" rm_name sign_char disp_val reg_name
          else
            Printf.printf "%s, [%s %c %d]\n" reg_name rm_name sign_char disp_val
      else if mod_val = mem_mod_no_disp then
        if is_two_reg_rm_field rm_val then
          let r1sum_name, r2sum_name = decode_eff_add_2 rm_val in
          if d = 0 then
            Printf.printf "[%s + %s], %s\n" r1sum_name r2sum_name reg_name
          else Printf.printf "%s, [%s + %s]\n" reg_name r1sum_name r2sum_name
        else if not @@ is_dir_add_rm_field rm_val then
          let rm_name = decode_eff_add_1 rm_val in
          if d = 0 then Printf.printf "[%s], %s\n" rm_name reg_name
          else Printf.printf "%s, [%s]\n" reg_name rm_name
        else
          let disp_lo = input_byte in_c in
          let disp_hi = input_byte in_c in
          let disp_val = convert_disp_unsigned_16 disp_lo disp_hi in
          if d = 0 then Printf.printf "[%d], %s\n" disp_val reg_name
          else Printf.printf "%s, [%d]\n" reg_name disp_val
      else
        let rm_name = decode_reg rm_val w in
        if d = 0 then Printf.printf "%s, %s\n" rm_name reg_name
        else Printf.printf "%s, %s\n" reg_name rm_name)
    else if are_equal_mask a test_imm_and_acc_opcode mask_l_7 then (
      Printf.printf "test ";
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      let acc_name = if w = 1 then "ax" else "al" in
      let data_lo = input_byte in_c in
      let data_val = data_lo + if w = 1 then input_byte in_c lsl 8 else 0 in
      Printf.printf "%s, %d\n" acc_name data_val)
    else if are_equal_mask a rep_opcode mask_l_7 then
      let z_shift, z_mask = (0, mask_r_1) in
      let z = field a z_shift z_mask in
      Printf.printf "%s " (if z = 1 then "rep" else "repnz")
    else if are_equal_mask a movs_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "movs%c\n" (if w = 1 then 'w' else 'b')
    else if are_equal_mask a cmps_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "cmps%c\n" (if w = 1 then 'w' else 'b')
    else if are_equal_mask a scas_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "scas%c\n" (if w = 1 then 'w' else 'b')
    else if are_equal_mask a lods_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "lods%c\n" (if w = 1 then 'w' else 'b')
    else if are_equal_mask a stos_opcode mask_l_7 then
      let w_shift, w_mask = (0, mask_r_1) in
      let w = field a w_shift w_mask in
      Printf.printf "stos%c\n" (if w = 1 then 'w' else 'b')
    else if are_equal_mask a call_direct_in_opcode mask_l_8 then
      let inc_lo = input_byte in_c in
      let inc_hi = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_16 (inc_lo + 3) inc_hi in
      Printf.printf "call $%c%d\n" sign_char inc_val
    else if are_equal_mask a call_direct_inter_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let ip_hi = input_byte in_c in
      let ip_val = ip_lo + (ip_hi lsl 8) in
      let cs_lo = input_byte in_c in
      let cs_hi = input_byte in_c in
      let cs_val = cs_lo + (cs_hi lsl 8) in
      Printf.printf "call %d:%d\n" cs_val ip_val
    else if are_equal_mask a jmp_direct_inter_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let ip_hi = input_byte in_c in
      let ip_val = ip_lo + (ip_hi lsl 8) in
      let cs_lo = input_byte in_c in
      let cs_hi = input_byte in_c in
      let cs_val = cs_lo + (cs_hi lsl 8) in
      Printf.printf "jmp %d:%d\n" cs_val ip_val
    else if are_equal_mask a jmp_direct_in_opcode mask_l_8 then
      let inc_lo = input_byte in_c in
      let inc_hi = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_16 (inc_lo + 3) inc_hi in
      Printf.printf "jmp $%c%d\n" sign_char inc_val
    else if are_equal_mask a jmp_direct_short_opcode mask_l_8 then
      let inc_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (inc_lo + 2) in
      Printf.printf "jmp $%c%d\n" sign_char inc_val
    else if are_equal_mask a ret_within_opcode mask_l_8 then print_endline "ret"
    else if are_equal_mask a ret_inter_opcode mask_l_8 then print_endline "retf"
    else if
      are_equal_mask a ret_within_imm_sp_opcode mask_l_8
      || are_equal_mask a ret_inter_imm_sp_opcode mask_l_8
    then
      let data_lo = input_byte in_c in
      let sign_char, data_val =
          convert_disp_signed_16 data_lo (input_byte in_c)
      in
      Printf.printf "ret %c%d\n" sign_char data_val
    else if are_equal_mask a jz_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jz $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnge_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnge $%c%d\n" sign_char inc_val
    else if are_equal_mask a jle_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jle $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnae_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnae $%c%d\n" sign_char inc_val
    else if are_equal_mask a jbe_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jbe $%c%d\n" sign_char inc_val
    else if are_equal_mask a jpe_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jpe $%c%d\n" sign_char inc_val
    else if are_equal_mask a jo_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jo $%c%d\n" sign_char inc_val
    else if are_equal_mask a js_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "js $%c%d\n" sign_char inc_val
    else if are_equal_mask a js_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "js $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnz_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnz $%c%d\n" sign_char inc_val
    else if are_equal_mask a jge_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jge $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnle_opode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnle $%c%d\n" sign_char inc_val
    else if are_equal_mask a jae_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jae $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnbe_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnbe $%c%d\n" sign_char inc_val
    else if are_equal_mask a jnp_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jnp $%c%d\n" sign_char inc_val
    else if are_equal_mask a jno_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jno $%c%d\n" sign_char inc_val
    else if are_equal_mask a jns_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jns $%c%d\n" sign_char inc_val
    else if are_equal_mask a loop_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "loop $%c%d\n" sign_char inc_val
    else if are_equal_mask a loopz_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "loopz $%c%d\n" sign_char inc_val
    else if are_equal_mask a loopnz_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "loopnz $%c%d\n" sign_char inc_val
    else if are_equal_mask a jcxz_opcode mask_l_8 then
      let ip_lo = input_byte in_c in
      let sign_char, inc_val = convert_disp_signed_8 (ip_lo + 2) in
      Printf.printf "jcxz $%c%d\n" sign_char inc_val
    else if are_equal_mask a int_type_spec_opcode mask_l_8 then
      let data_val = input_byte in_c in
      Printf.printf "int %d\n" data_val
    else if are_equal_mask a int_type_3_opcode mask_l_8 then
      print_endline "int3"
    else if are_equal_mask a into_opcode mask_l_8 then print_endline "into"
    else if are_equal_mask a iret_opcode mask_l_8 then print_endline "iret"
    else if are_equal_mask a clc_opcode mask_l_8 then print_endline "clc"
    else if are_equal_mask a cmc_opcode mask_l_8 then print_endline "cmc"
    else if are_equal_mask a stc_opcode mask_l_8 then print_endline "stc"
    else if are_equal_mask a cld_opcode mask_l_8 then print_endline "cld"
    else if are_equal_mask a std_opcode mask_l_8 then print_endline "std"
    else if are_equal_mask a cli_opcode mask_l_8 then print_endline "cli"
    else if are_equal_mask a sti_opcode mask_l_8 then print_endline "sti"
    else if are_equal_mask a hlt_opcode mask_l_8 then print_endline "hlt"
    else if are_equal_mask a wait_opcode mask_l_8 then print_endline "wait"
    else if are_equal_mask a esc_opcode mask_l_5 then (
      print_endline "; esc (not implemented)";
      let b = input_byte in_c in
      let mod_shift, mod_mask = (6, mask_r_2) in
      let mod_val = field b mod_shift mod_mask in
      let rm_shift, rm_mask = (0, mask_r_3) in
      let rm_val = field b rm_shift rm_mask in
      if mod_val = mem_mod_8_disp then
        let _disp_lo = input_byte in_c in
        ()
      else if mod_val = mem_mod_16_disp then
        let _disp_lo = input_byte in_c in
        let _disp_hi = input_byte in_c in
        ()
      else if mod_val = mem_mod_no_disp then
        if (not @@ is_two_reg_rm_field rm_val) && is_dir_add_rm_field rm_val
        then
          let _disp_lo = input_byte in_c in
          let _disp_hi = input_byte in_c in
          ())
    else if are_equal_mask a lock_opcode mask_l_8 then print_string "lock "
    else if are_equal_mask a segment_opcode (mask_l_3 lor mask_r_3) then
      let seg_shift, seg_mask = (3, mask_r_2) in
      let seg_val = field a seg_shift seg_mask in
      seg_prefix_val := seg_val
    else Printf.fprintf stderr "Error: wrong opcode %X\n" a

(**[disassemble_file filename] opens file [filename] and tries to read bytes
   from it, to disassemble as many 80x86 assembly instructions as can be read.
   Disassembled instructions are printed to standard ouput.*)
let disassemble_file filename =
    print_endline "bits 16\n";
    let in_c = open_in_bin filename in
    let seg_prefix_val = ref (-1) in
    try
      while true do
        disassemble_instr in_c seg_prefix_val
      done
    with End_of_file -> ()

let () =
    if Array.length Sys.argv <= 1 then
      failwith "No file name specified. Aborting."
    else disassemble_file Sys.argv.(1)
