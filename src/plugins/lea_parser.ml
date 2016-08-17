(* this module is used to parse calculations in lea's experand *)

(* Note that in the main engine of Uroboros, we actually don't need this, *)
(* however, for some data flow analysis task, we have to do this. *)

(* I would like to translate typical instructions into the following
representation:

      lea 0x0(%esp), %esp

into:

      add %esp 0x0 t
      mov t %esp

If you want to use this module, please define a temporary register "t" (as what
I did in the symbolic/engine.py )
 *)
module Lea_parser = struct

    open Type
    open Ail_utils
    open Pp_print

    let translate e1 e2 l =
      match e2 with
        | Label s ->
           (* TODO: lea S_0xasdasd, %esi *)
           [
             (TripleInstr (CommonOP (Assign MOV), e1, Label s,
                           l, None));
           ]
        | Const (Point i) ->
           [
             (TripleInstr (CommonOP (Assign MOV), e1, Const (Normal i),
                           l, None));
           ]
        | Ptr (UnOP (r)) ->
           [
             (TripleInstr (CommonOP (Assign MOV), e1, Reg r,
                           l, None));
           ]
        | Ptr (BinOP_PLUS (r, i)) ->
           [
             (FourInstr (CommonOP (Arithm ADD), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS_S (r, str_i)) ->
           [
             (FourInstr (CommonOP (Arithm ADD), Label "%t0",
                         Label ("$"^str_i), Reg r, l, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_MINUS (r, i)) ->
           [
             (FourInstr (CommonOP (Arithm SUB), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (ThreeOP (r1, r2, i)) ->
           [
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int i), Reg r2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Arithm ADD), Label "%t0", Reg r1,
                         Label "%t0",  {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov i2(r1,r2,i1),%edx *)
        (* this equals to r1 + i2 + (r2 * i1) *)
        | Ptr (FourOP_PLUS (r1 , r2 , i1 , i2)) ->
           [
             (FourInstr (CommonOP (Arithm ADD), Label "%t1",
                         Label (string_of_int i2), Reg r1, l, None));
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int i1), Reg r2, {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", Label "%t1",
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov i2(r1,r2,i1),%edx *)
        (* this equals to r1 + i2 + (r2 * i1) *)
        | Ptr (FourOP_PLUS_S (r1 , r2 , i1 , str_i2)) ->
           [
             (FourInstr (CommonOP (Arithm ADD), Label "%t1",
                         Label (str_i2), Reg r1, l, None));
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int i1), Reg r2, {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", Label "%t1",
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov -i2(r1,r2,i1),%edx *)
        (* this equals to r1 - i2 + (r2 * i1) *)
        | Ptr (FourOP_MINUS_S (r1, r2, i1, str_i2)) ->
           [
             (FourInstr (CommonOP (Arithm SUB), Label "%t1",
                         Label (str_i2), Reg r1, l, None));
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int i1), Reg r2, {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", Label "%t1",
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov -i2(r1,r2,i1),%edx *)
        (* this equals to r1 - i2 + (r2 * i1) *)
        | Ptr (FourOP_MINUS (r1, r2, i1, i2)) ->
           [
             (FourInstr (CommonOP (Arithm SUB), Label "%t1",
                         Label (string_of_int i2), Reg r1, l, None));
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int i1), Reg r2, {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", Label "%t1",
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov 0x123(,%ebx,4),%eax *)
        | Ptr (JmpTable_PLUS (base, r, offset)) ->
           [
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int offset), Reg r, l, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", (Const (Normal base)),
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov -0x123(,%ebx,4),%eax *)
        | Ptr (JmpTable_MINUS (base, r, offset)) ->
           [
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int offset), Reg r, l, None));
             (TripleInstr (CommonOP (Arithm SUB), Label "%t0", (Const (Normal base)),
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov S_0x8048440(,%ebx,4),%eax *)
        | Ptr (JmpTable_PLUS_S (str_base, r, offset)) ->
           [
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int offset), Reg r, l, None));
             (TripleInstr (CommonOP (Arithm ADD), Label "%t0", Label str_base,
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        (* mov -S_0x8048440(,%ebx,4),%eax *)
        | Ptr (JmpTable_MINUS_S (str_base, r, offset)) ->
           [
             (FourInstr (CommonOP (Arithm MUL), Label "%t0",
                         Label (string_of_int offset), Reg r, l, None));
             (TripleInstr (CommonOP (Arithm SUB), Label "%t0", Label str_base,
                           {l with loc_label = ""}, None));
             (TripleInstr (CommonOP (Assign MOV), e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | _ ->
           begin
             let _ = print_endline @@ dec_hex l.loc_addr in
             failwith "undefined lea instruction"
           end


    let process i =
      match i with
      | TripleInstr (CommonOP (Assign LEA), e1, e2, l, _) -> translate e1 e2 l
      | _ -> failwith "undefined lea instruction"


  end
