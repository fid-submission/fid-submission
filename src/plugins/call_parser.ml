(* this module is used to translate control transfer opcode*)

(* for some data flow analysis task, we have to do this. *)

(* I would like to translate typical instructions into the following
representation:

call X

into:

push ret_label
jmp X

ret

into:

pop ecx; jmp ecx

 *)

module Call_parser = struct

    open Type
    open Ail_utils
    open Pp_print


    (* ret/retn  ==> pop ecx; jmp ecx
     *   EAX, ECX and EDX are caller save registers.
     *   EBP, EBX, EDI and ESI are callee save registers.
     *)
    let update_ret i =
      let l = get_loc i in
      let l' = {l with loc_label = ""} in
      let i2 =
        DoubleInstr (StackOP POP, Reg (CommonReg ECX), l, None)
      in
      let i1 =
        DoubleInstr (ControlOP (Jump JMP), Symbol (StarDes (Reg (CommonReg ECX)
                                     )), l', None)
      in
      [i1; i2]

    let update_retq i =
      let l = get_loc i in
      let l' = {l with loc_label = ""} in
      let i2 =
        DoubleInstr (StackOP POP, Reg (CommonReg RCX), l, None)
      in
      let i1 =
        DoubleInstr (ControlOP (Jump JMP), Symbol (StarDes (Reg (CommonReg ECX)
                                     )), l', None)
      in
      [i1; i2]


    let update_call i =
      let l = get_loc i in
      let e = get_exp_1 i in
      let jmp_label = "S_"^(dec_hex l.loc_addr)^"_next" in
      let l1 = {l with loc_label = ""} in
      let i2 =
        DoubleInstr (StackOP PUSH, Label ("$"^jmp_label), l, None)
      in
      let i3 =
        DoubleInstr (ControlOP (Jump JMP), e, l1, None)
      in
      let i1 =
        SingleInstr (CommonOP (Other NOP)
                    , {l with loc_label = jmp_label^": "}, None)
      in
      [i2; i3; i1]


    let process i =
      match get_op i with
      | ControlOP (CALL)
        -> update_call i
      | ControlOP (CALLQ) -> update_call i
      | ControlOP (RET)
        -> update_ret i
      | ControlOP (RETN) -> failwith "undefined retn"
      | ControlOP (RETQ)
        -> update_ret i
      | _ -> failwith "undefined opcode"


end
