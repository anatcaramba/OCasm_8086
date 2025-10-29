val mask_r_1 : int
(** Mask byte values such that [mask_r_k] checks is mask consisting of [k] 1s to
    the right, else 0s.*)

val mask_r_2 : int
(** Mask byte values such that [mask_r_k] checks is mask consisting of [k] 1s to
    the right, else 0s.*)

val mask_r_3 : int
(** Mask byte values such that [mask_r_k] checks is mask consisting of [k] 1s to
    the right, else 0s.*)

type printing_t =
    | Single of string
    | Segment
    | Escape
    | Relative_disp of string
    | Integer_value
    | Direct_inter of string
    | Direct_in of string
    | Jmp_direct_short
    | Single_width of string
    | Ret_imm_sp
    | Rep
    | Mov_imm_reg
    | Test_imm_acc
    | Test_xchg_rm_reg of string
    | Rm_reg of string
    | Lea_les_lds of string
    | Arith_rm_reg
    | Shift_rotate
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
        (** Type representing all the different kinds of instruction formats
            (and output strings), grouped by similarity.*)

type opcode_t = { codebyte : int; mask : int; printing : printing_t }
(** An opcode is given by the value [codebyte] to be looked for in the first
    byte, along with the [mask] it should be matched with when testing for said
    opcode. It also provides the corresponding [printing_t] variant [printing]
    associated with the opcode. *)

val opcodes_array : opcode_t array
(**Contains all different [opcode_t] available. It is not sorted in any way.*)
