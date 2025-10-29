(** Mask byte values such that [mask_l_k] checks is mask consisting of [k] 1s to
    the left, else 0s.*)
let mask_l_8, mask_l_7, mask_l_6, mask_l_5, mask_l_4, mask_l_3 =
    (0XFF, 0XFE, 0XFC, 0XF8, 0XF0, 0xE0)

(** Mask byte values such that [mask_r_k] checks is mask consisting of [k] 1s to
    the right, else 0s.*)
let mask_r_3, mask_r_2, mask_r_1 = (0X07, 0X03, 0X01)

(* type printout_t = int *)

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
let test_rm_and_reg_opcode = 0X84

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

type printing_t =
    | Single of string
    (*single instruction string*)
    | Segment
    | Escape
    | Relative_disp of string
    (*$+n*)
    | Integer_value
    | Direct_inter of string
    (*val:val*)
    | Direct_in of string
    | Jmp_direct_short
    | Single_width of string
    (*movsb, movsw*)
    | Ret_imm_sp
    | Rep
    (*rep, repnz and maybe others*)
    | Mov_imm_reg
    | Test_imm_acc
    | Test_xchg_rm_reg of string
    | Rm_reg of string
    (* mov rm and reg, and arith rm and reg seem to be absolutely similar. To check. ALso les, lds, lea*)
    | Lea_les_lds of string
    | Arith_rm_reg
    (*similar to [Rm_reg] but argument is not decidable from opcode.*)
    | Shift_rotate
      (*different from previous? not really. only difference is register at the end, or 1. Maybe group as 1*)
    | Neg_mul_div_not_test
    | Inc_dec_push_pop_reg of string
    | Arith_imm_rm
    | Arith_imm_acc
    | Out_to_var
    | Out_to_fixed
    | In_from_var
    | In_from_fixed
    | Xchg_reg_acc
    | Push_pop_rm of string
    | Inc_dec_rm
    | Mov_mem_to_acc
    | Mov_acc_to_mem
    | Mov_imm_to_rm
    | Push_pop_seg_reg of string
    | Aam_aad of string
    | Mov_rm_to_seg_reg
    | Mov_seg_reg_to_rm

type opcode_t = { codebyte : int; mask : int; printing : printing_t }

let opcodes_array =
    [|
      {
        codebyte = mov_reg_tofr_rm_opcode;
        mask = mask_l_6;
        printing = Rm_reg "mov";
      };
      {
        codebyte = mov_imm_to_rm_opcode;
        mask = mask_l_7;
        printing = Mov_imm_to_rm;
      };
      {
        codebyte = mov_imm_to_reg_opcode;
        mask = mask_l_4;
        printing = Mov_imm_reg;
      };
      {
        codebyte = mov_mem_to_acc_opcode;
        mask = mask_l_7;
        printing = Mov_mem_to_acc;
      };
      {
        codebyte = mov_acc_to_mem_opcode;
        mask = mask_l_7;
        printing = Mov_acc_to_mem;
      };
      {
        codebyte = mov_rm_to_seg_reg_opcode;
        mask = mask_l_8;
        printing = Mov_rm_to_seg_reg;
      };
      {
        codebyte = mov_seg_reg_to_rm_opcode;
        mask = mask_l_8;
        printing = Mov_seg_reg_to_rm;
      };
      {
        codebyte = push_reg_opcode;
        mask = mask_l_5;
        printing = Inc_dec_push_pop_reg "push";
      };
      {
        codebyte = push_seg_reg_opcode;
        mask = mask_l_3 lor mask_r_3;
        printing = Push_pop_seg_reg "push";
      };
      {
        codebyte = pop_rm_opcode;
        mask = mask_l_8;
        printing = Push_pop_rm "pop";
      };
      {
        codebyte = pop_reg_opcode;
        mask = mask_l_5;
        printing = Inc_dec_push_pop_reg "pop";
      };
      {
        codebyte = pop_seg_reg_opcode;
        mask = mask_l_3 lor mask_r_3;
        printing = Push_pop_seg_reg "pop";
      };
      {
        codebyte = inc_or_dec_rm_opcode;
        mask = mask_l_7;
        printing = Inc_dec_rm;
      };
      {
        codebyte = xchg_rm_reg_opcode;
        mask = mask_l_7;
        printing = Test_xchg_rm_reg "xchg";
      };
      {
        codebyte = xchg_reg_acc_opcode;
        mask = mask_l_5;
        printing = Xchg_reg_acc;
      };
      {
        codebyte = in_from_fixed_opcode;
        mask = mask_l_7;
        printing = In_from_fixed;
      };
      {
        codebyte = in_from_variable_opcode;
        mask = mask_l_7;
        printing = In_from_var;
      };
      {
        codebyte = out_to_fixed_opcode;
        mask = mask_l_7;
        printing = Out_to_fixed;
      };
      {
        codebyte = out_to_variable_opcode;
        mask = mask_l_7;
        printing = Out_to_var;
      };
      { codebyte = xlat_opcode; mask = mask_l_8; printing = Single "xlat\n" };
      { codebyte = lahf_opcode; mask = mask_l_8; printing = Single "lahf\n" };
      { codebyte = sahf_opcode; mask = mask_l_8; printing = Single "sahf\n" };
      { codebyte = pushf_opcode; mask = mask_l_8; printing = Single "pushf\n" };
      { codebyte = popf_opcode; mask = mask_l_8; printing = Single "popf\n" };
      { codebyte = lea_opcode; mask = mask_l_8; printing = Lea_les_lds "lea" };
      { codebyte = lds_opcode; mask = mask_l_8; printing = Lea_les_lds "lds" };
      { codebyte = les_opcode; mask = mask_l_8; printing = Lea_les_lds "les" };
      {
        codebyte = arith_rm_with_reg_opcode;
        mask = 0XC4;
        printing = Arith_rm_reg;
      };
      {
        codebyte = arith_imm_tofr_rm_opcode;
        mask = mask_l_6;
        printing = Arith_imm_rm;
      };
      {
        codebyte = arith_imm_to_acc_opcode;
        mask = 0XC6;
        printing = Arith_imm_acc;
      };
      {
        codebyte = inc_reg_opcode;
        mask = mask_l_5;
        printing = Inc_dec_push_pop_reg "inc";
      };
      { codebyte = aaa_opcode; mask = mask_l_8; printing = Single "aaa\n" };
      { codebyte = daa_opcode; mask = mask_l_8; printing = Single "daa\n" };
      {
        codebyte = dec_reg_opcode;
        mask = mask_l_5;
        printing = Inc_dec_push_pop_reg "dec";
      };
      {
        codebyte = neg_mul_div_not_test_opcode;
        mask = mask_l_7;
        printing = Neg_mul_div_not_test;
      };
      { codebyte = aas_opcode; mask = mask_l_8; printing = Single "aas\n" };
      { codebyte = das_opcode; mask = mask_l_8; printing = Single "das\n" };
      { codebyte = cbw_opcode; mask = mask_l_8; printing = Single "cbw\n" };
      { codebyte = cwd_opcode; mask = mask_l_8; printing = Single "cwd\n" };
      { codebyte = aam_opcode; mask = mask_l_8; printing = Aam_aad "aam" };
      { codebyte = aad_opcode; mask = mask_l_8; printing = Aam_aad "aad" };
      {
        codebyte = shift_rotate_opcode;
        mask = mask_l_6;
        printing = Shift_rotate;
      };
      {
        codebyte = test_imm_and_acc_opcode;
        mask = mask_l_7;
        printing = Test_imm_acc;
      };
      { codebyte = rep_opcode; mask = mask_l_7; printing = Rep };
      {
        codebyte = movs_opcode;
        mask = mask_l_7;
        printing = Single_width "movs";
      };
      {
        codebyte = cmps_opcode;
        mask = mask_l_7;
        printing = Single_width "cmps";
      };
      {
        codebyte = scas_opcode;
        mask = mask_l_7;
        printing = Single_width "scas";
      };
      {
        codebyte = lods_opcode;
        mask = mask_l_7;
        printing = Single_width "lods";
      };
      {
        codebyte = stos_opcode;
        mask = mask_l_7;
        printing = Single_width "stos";
      };
      {
        codebyte = call_direct_in_opcode;
        mask = mask_l_8;
        printing = Direct_in "call";
      };
      {
        codebyte = call_direct_inter_opcode;
        mask = mask_l_8;
        printing = Direct_inter "call";
      };
      {
        codebyte = jmp_direct_in_opcode;
        mask = mask_l_8;
        printing = Direct_in "jmp";
      };
      {
        codebyte = jmp_direct_short_opcode;
        mask = mask_l_8;
        printing = Jmp_direct_short;
      };
      {
        codebyte = jmp_direct_inter_opcode;
        mask = mask_l_8;
        printing = Direct_inter "jmp";
      };
      {
        codebyte = ret_within_opcode;
        mask = mask_l_8;
        printing = Single "ret\n";
      };
      {
        codebyte = ret_inter_opcode;
        mask = mask_l_8;
        printing = Single "retf\n";
      };
      {
        codebyte = ret_within_imm_sp_opcode;
        mask = mask_l_8;
        printing = Ret_imm_sp;
      };
      {
        codebyte = ret_inter_imm_sp_opcode;
        mask = mask_l_8;
        printing = Ret_imm_sp;
      };
      { codebyte = jz_opcode; mask = mask_l_8; printing = Relative_disp "jz" };
      {
        codebyte = jnge_opcode;
        mask = mask_l_8;
        printing = Relative_disp "jnge";
      };
      { codebyte = jle_opcode; mask = mask_l_8; printing = Relative_disp "jle" };
      {
        codebyte = jnae_opcode;
        mask = mask_l_8;
        printing = Relative_disp "jnae";
      };
      { codebyte = jbe_opcode; mask = mask_l_8; printing = Relative_disp "jbe" };
      { codebyte = jpe_opcode; mask = mask_l_8; printing = Relative_disp "jpe" };
      { codebyte = jo_opcode; mask = mask_l_8; printing = Relative_disp "jo" };
      { codebyte = js_opcode; mask = mask_l_8; printing = Relative_disp "js" };
      { codebyte = jnz_opcode; mask = mask_l_8; printing = Relative_disp "jnz" };
      { codebyte = jge_opcode; mask = mask_l_8; printing = Relative_disp "jge" };
      {
        codebyte = jnle_opode;
        mask = mask_l_8;
        printing = Relative_disp "jnle";
      };
      { codebyte = jae_opcode; mask = mask_l_8; printing = Relative_disp "jae" };
      {
        codebyte = jnbe_opcode;
        mask = mask_l_8;
        printing = Relative_disp "jnbe";
      };
      { codebyte = jnp_opcode; mask = mask_l_8; printing = Relative_disp "jnp" };
      { codebyte = jno_opcode; mask = mask_l_8; printing = Relative_disp "jno" };
      { codebyte = jns_opcode; mask = mask_l_8; printing = Relative_disp "jns" };
      {
        codebyte = loop_opcode;
        mask = mask_l_8;
        printing = Relative_disp "loop";
      };
      {
        codebyte = loopz_opcode;
        mask = mask_l_8;
        printing = Relative_disp "loopz";
      };
      {
        codebyte = loopnz_opcode;
        mask = mask_l_8;
        printing = Relative_disp "loopnz";
      };
      {
        codebyte = jcxz_opcode;
        mask = mask_l_8;
        printing = Relative_disp "jcxz";
      };
      {
        codebyte = int_type_spec_opcode;
        mask = mask_l_8;
        printing = Integer_value;
      };
      {
        codebyte = int_type_3_opcode;
        mask = mask_l_8;
        printing = Single "int3\n";
      };
      { codebyte = into_opcode; mask = mask_l_8; printing = Single "into\n" };
      { codebyte = iret_opcode; mask = mask_l_8; printing = Single "iret\n" };
      { codebyte = clc_opcode; mask = mask_l_8; printing = Single "clc\n" };
      { codebyte = cmc_opcode; mask = mask_l_8; printing = Single "cmc\n" };
      { codebyte = stc_opcode; mask = mask_l_8; printing = Single "stc\n" };
      { codebyte = cld_opcode; mask = mask_l_8; printing = Single "cld\n" };
      { codebyte = std_opcode; mask = mask_l_8; printing = Single "std\n" };
      { codebyte = cli_opcode; mask = mask_l_8; printing = Single "cli\n" };
      { codebyte = sti_opcode; mask = mask_l_8; printing = Single "sti\n" };
      { codebyte = hlt_opcode; mask = mask_l_8; printing = Single "hlt\n" };
      { codebyte = wait_opcode; mask = mask_l_8; printing = Single "wait " };
      { codebyte = esc_opcode; mask = mask_l_5; printing = Escape };
      { codebyte = lock_opcode; mask = mask_l_8; printing = Single "lock " };
      {
        codebyte = segment_opcode;
        mask = mask_l_3 lor mask_r_3;
        printing = Segment;
      };
      {
        codebyte = test_rm_and_reg_opcode;
        mask = mask_l_7;
        printing = Test_xchg_rm_reg "test";
      };
    |]

(* let () = Array.sort compare opcodes_array *)
