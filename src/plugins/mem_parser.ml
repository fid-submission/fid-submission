(* this module is used to parse calculations in memory access experand *)

(* Note that in the main engine of Uroboros, we actually don't need this, *)
(* however, for some data flow analysis task, we have to do this. *)

(* I would like to translate typical instructions into the following
representation:

      OP 0x0(%esp), %esp

into:

      add %esp 0x0 t
      OP t %esp

If you want to use this module, please define a temporary register "t" (as what
I did in the symbolic/engine.py )

 *)

(* TODO:

1. currently we only parser the this experand as this is strongly related to
      stack access.
      off(%reg)

We need more later.

 *)



module Mem_parser = struct

    open Type
    open Ail_utils

    let aux i =
      String.sub i 2 (String.length i-2)
      |> int_of_string
      |> string_of_int

   (* mov 0x3c(%esp),%eax *)
    let translate_load op e1 e2 l i =
      let open Opcode_utils in
      match e2 with
        | Ptr (BinOP_PLUS (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS_S (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label (aux i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS_S (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label (aux i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_MINUS (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label ("-"^string_of_int i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_MINUS (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label ("-"^string_of_int i), Reg r, l, None));
             (TripleInstr (op, e1, Label "%t0",
                           {l with loc_label = ""}, None));
           ]
        | _ -> [i]


   (* mov %rdi, 0x3c(%esp) *)
    let translate_store op e1 e2 l i =
      let open Opcode_utils in
      match e1 with
        | Ptr (BinOP_PLUS (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STOREL), Label (string_of_int i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS_S (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOADL), Label "%t0",
                         Label (aux i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STOREL), Label (aux i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS_S (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label (aux i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STOREL), Label (aux i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_PLUS (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label (string_of_int i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STORE), Label (string_of_int i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_MINUS (r, i)) when is_long_opcode op = true ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label ("-"^string_of_int i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STOREL), Label ("-"^string_of_int i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | Ptr (BinOP_MINUS (r, i)) ->
           [
             (FourInstr (CommonOP (Other LOAD), Label "%t0",
                         Label ("-"^string_of_int i), Reg r, l, None));
             (TripleInstr (op, Label "%t0", e2, {l with loc_label = ""}, None));
             (FourInstr (CommonOP (Other STORE), Label ("-"^string_of_int i),
                         Reg r, Label "%t0", {l with loc_label = ""}, None));
           ]
        | _ -> [i]


    let process i =
      let aux i =
        let open Exp_utils in
        match i with
        (* TODO: we now only catch triple instruction, as this is where stack
      access happens
         *)
        | TripleInstr (op, e1, e2, l, _) when is_binop e2 ->
           translate_load op e1 e2 l i
        (* translate store operations *)
        | TripleInstr (op, e1, e2, l, _) when is_binop e1 ->
           translate_store op e1 e2 l i
        | _ -> [i]
      in
      aux i


  end
