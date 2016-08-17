open Type

class stack_of_loc =
    object (self)
      val mutable the_list : loc list = []
      method push x =
        the_list <- x :: the_list
      method pop =
        let result = List.hd the_list in
        the_list <- List.tl the_list;
        result
      method peek =
        List.hd the_list
      method size =
        List.length the_list
end

module Symbolic_engine = struct

    open Ail_utils
    open Lifter

(* this module symbolic execute instructions sequences *)

    let e_idx = ref (-1)
    let e_stk = ref []

    let gen_symb () =
      "symb_" ^ (string_of_int !e_idx)


    let store_equation (e : string) =
      e_stk := e::!e_stk;
      e_idx := !e_idx + 1


    let set_r_ctx r_ctx r e =
      store_equation e;
      Hashtbl.replace r_ctx r !e_idx


    let get_r_ctx r_ctx r =
      Hashtbl.find r_ctx r
      |> int_of_string
      |> List.nth !e_stk



    let init_context () =
      (* initialize context; including both register and memory *)
      (* TODO: stack and heap? *)
      let r_ctx = Hashtbl.create 10 in
      Hashtbl.replace r_ctx "%eax" "";
      Hashtbl.replace r_ctx "%ebx" "";
      Hashtbl.replace r_ctx "%ecx" "";
      Hashtbl.replace r_ctx "%edx" "";
      Hashtbl.replace r_ctx "%edi" "";
      Hashtbl.replace r_ctx "%esi" "";
      Hashtbl.replace r_ctx "%esp" "";
      Hashtbl.replace r_ctx "%ebp" "";

      let s_ctx = new stack_of_loc in
      (r_ctx, s_ctx)


    let execute_mov r_ctx e1 e2 =
      let module EL = Exp_lifter in
      let open Pp_print in
      match (EL.lift e1, EL.lift e2) with
        | (EL.EXP_REG, EL.EXP_CONST) ->
           let es2 = p_exp e2 in
           let es1 = p_exp e1 in
           print_endline es1;
           print_endline es2;
           Hashtbl.replace r_ctx es1 es2
        | (EL.EXP_REG, EL.EXP_REG) ->
           let es2 = p_exp e2 in
           let es1 = p_exp e1 in
           let r = Hashtbl.find r_ctx es1 in
           Hashtbl.replace r_ctx es2 r
        | (EL.EXP_MEM, EL.EXP_REG) ->
           let mem = p_exp e2 in
           let reg = p_exp e1 in
           let s = gen_symb () in
           store_equation s;
           Hashtbl.replace r_ctx reg @@ string_of_int !e_idx
        | (_, _) ->
           ()




    let execute_add r_ctx e1 e2 =
      let module EL = Exp_lifter in
      let open Pp_print in
      (* this is the src *)
      match (EL.lift e1, EL.lift e2) with
        | (EL.EXP_CONST, EL.EXP_REG) ->
           let es2 = p_exp e2 in
           let es1 = p_exp e1 in
           Hashtbl.replace r_ctx es2 es1
        | (EL.EXP_REG, EL.EXP_REG) ->
           let es2 = p_exp e2 in
           let es1 = p_exp e1 in
           let r = Hashtbl.find r_ctx es2 in
           Hashtbl.replace r_ctx es2 r
        | _ ->
           ()


    let simplify eq =
      eq


    let get_reg_equation r_ctx r =
      Hashtbl.find r_ctx r
      |> int_of_string
      |> List.nth !e_stk



    let get_reg_equation_simplified r =
      get_reg_equation r
      |> simplify



    let execute_triple r_ctx p e1 e2 =
      let module OL = OP_lifter in
      match OL.lift p with
        | OL.OP_MOV ->
           execute_mov r_ctx e1 e2;
           ()
        | OL.OP_ADD ->
           execute_add r_ctx e1 e2;
           ()
        | OL.OP_UNDEFINED -> ()
        | _ -> ()



    let execute r_ctx s_ctx i =
      (* TODO: change this with ir *)
      match i with
        (* TODO: besides NOP, do we have any other single instruction? *)
      | SingleInstr (p, l, _) -> i
      | DoubleInstr (p, exp1, l, _) -> i
      | TripleInstr (p, e1, e2, l, _) ->
         execute_triple r_ctx p e1 e2;
         i
      (* TODO: mul and sub could have three operands *)
      | FourInstr (p, exp1, exp2, exp3, l, _) -> i



    let process il =
      let (r_ctx, s_ctx) = init_context () in
      let aux i =
        execute r_ctx s_ctx i
      in
      let il' =
      il
      |> List.map aux
      |> List.rev
      in
      print_endline @@ (Hashtbl.find r_ctx "%eax");
     (*   print_string @@ get_reg_equation r_ctx "%eax"; *)
      il'



  end
