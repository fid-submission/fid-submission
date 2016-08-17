exception UndefinedIR of string

open Type


module Stack_Change = struct
    (* this module rewrites stack opcode into an explicit format *)

    open Ail_utils


    let translate p e l i =
      match p with
        | StackOP PUSH | StackOP PUSHL ->
           [
             TripleInstr (CommonOP (Assign MOVL), Ptr (UnOP (StackReg ESP)), e, l, None);
                         (TripleInstr (CommonOP (Arithm SUB), Reg (StackReg ESP),
                                       Const (Normal 4), {l with loc_label = ""}, None))
           ]
        | StackOP POP | StackOP POPL ->
           [
             TripleInstr (CommonOP (Assign MOVL), e, Ptr (UnOP (StackReg ESP)), l, None);
                         (TripleInstr (CommonOP (Arithm ADD), Reg (StackReg ESP),
                                       Const (Normal 4), {l with loc_label = ""}, None))
           ]
        | ControlOP LEAVE ->
           [
             TripleInstr (CommonOP (Assign MOVL), Reg (StackReg EBP), Reg (StackReg ESP), l, None);
             TripleInstr (CommonOP (Assign MOVL), Reg (StackReg EBP),
                          Ptr (UnOP (StackReg ESP)), {l with loc_label = ""}, None);
                         (TripleInstr (CommonOP (Arithm ADD), Reg (StackReg ESP),
                                      Const (Normal 4), {l with loc_label = ""}, None))
           ]
             (* TODO: FIXME *)
        | StackOP PUSHF | StackOP POPF -> [i]
        |_ -> raise (UndefinedIR "stack change")


    let stack i =
      let module OU = Opcode_utils in
      match (OU.is_stack_op @@ get_op i) with
      | true ->
         begin
           match i with
           | SingleInstr (p, l, _) ->
              translate p (Const (Normal 1)) l i
           | DoubleInstr (p, exp, l, _) ->
              translate p exp l i
           | _ -> failwith "error in stack"
         end
      | false -> failwith "error in stack translation"

end

module Flag = struct
  (* this module reveals the CPU flags asscoiated with each opcode. *)

    (* TODO: actually I haven't decided this part, make it before
    or after lifting ? *)


    let trans_common p =
      []


    let trans_control p =
      []


    let translate p =
      match p with
        | CommonOP p' ->
           trans_common p'
        | ControlOP p' ->
           trans_control p'
        | SystemOP _ -> []
        | StackOP _ -> []
        | ErrorOP _ -> []


end


module OP_lifter = struct


    type op_lifter = OP_MOV | OP_ADD | OP_SUB | OP_RET | OP_UNDEFINED


    open Ail_utils


    let lift p =
      let module OU = Opcode_utils in
      if OU.is_mov p then
        OP_MOV
      else
        OP_UNDEFINED


end

module Exp_lifter = struct


    open Ail_utils

    type exp_lifter = EXP_REG | EXP_CONST | EXP_MEM | EXP_UNDEFINED


    let lift p =
      let module EU = Exp_utils in
      if EU.is_reg p then
        EXP_REG
      else if EU.is_const p then
        EXP_CONST
      else if EU.is_mem p then
        EXP_MEM
      else
        EXP_UNDEFINED



end


module Lifter = struct
    (*
    this module lift uroboro's intermediate representation (IR) to the
    intermediate language(IL)
     *)

    open Ail_utils

    let lift i =
      let module SC = Stack_Change in
      let module OU = Opcode_utils in
      match i with
      | SingleInstr (p, l, _) when OU.is_stack_op p ->
        SC.stack i
      | DoubleInstr (p, exp1, l, _) when OU.is_stack_op p ->
        SC.stack i
      | SingleInstr (p, l, _) -> [i;]
      | DoubleInstr (p, exp1, l, _) -> [i;]
      | TripleInstr (p, exp1, exp2, l, _) -> [i;]
      | FourInstr (p, exp1, exp2, exp3, l, _) -> [i;]


    let lift_il il =
      il
      |> List.map lift
      |> List.rev
      |> List.flatten


end
