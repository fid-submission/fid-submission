open Type
open Pp_print

open Ail_utils

       (* 2014-12-06 :
        * this code was intended to be used in data flow anaysis for
        * symbol/memmory reference inference, but it could be abondant now.
        *)

       (* see the dataflow.ml for AIL's data flow analysis framework *)
(*
*     dataflow analysis
*     It is not acceptable to only leverage heriustic analysis above to identify symbols
*     Let's do dataflow analysis on identified values by heriustic analysis.
*     Be specially, on each identified value, initially it could be used in three ways
*       1. assign on top of the stack
*             push 0x401058
*             ret
*       we don't consider this situation right now
*       2. assign to registers
*             mov eax, 0x8041020
*             ...
*             call *eax
*       for this situation, we **linearly** process the next 50 instructions,
*         (taking into account value propagation by registers)
*       until
*             1. register is used as pointer
*
*       Too bad, even though we have a coarse-grained CFG, but let's just do it linearly now
*       3. global variable
*              mov DWORD PTR 0x424a38, 0x424a30
*              mov ecx, DWORD PTR 0x424a38
*              mov DWORD PTR [ecx],0x4
*
*              mov DWORD PTR 0x424a3c, 0x401032
*              call DWORD PTR 0x424a3c
*       It seems that we have to keep track of any "tainted" global variables
*       4. heap..?
*)


class dataFlow (instr_list : instr list) =
      let is_assign_op = function
          | CommonOP op' ->
            begin
              match op' with
              | Assign _ -> true
              | _ -> false
            end
          | _ -> false

      and is_arithmetic_op = function
          | CommonOP op' ->
            begin
              match op' with
              | Arithm _ -> true
              | _ -> false
            end
          | _ -> false

      and is_const = function
          | Const c ->
              (
                match c with
                | Normal _ -> true
                | _ -> false
              )
          | _ -> false

      and is_call_jmp = function
        | ControlOP op ->
          begin
            match op with
            | Jump _ -> true
            | CALL -> true
            | _ -> false
          end
        | _ -> false

      and is_star_ptr = function
      | Symbol s ->
        begin
          match s with
          |StarDes e -> Some e
          | _ -> None
        end
      | _ -> None

      and is_reg = function
          | Reg _ -> true (* mov $0x804844b,%eax *)
          | _ -> false

      (*
       * here is the finding, it seems that we have to simulate the stack, because
       * stack can be accessed by both stack register or pop/push to/from common register
       *)
      and is_ptr  = function
          | Ptr _ -> true (* mov $0x804844b,0x14(%esp) *)
          | _ -> false in

      let is_propagation_container e =
        is_reg e || is_ptr e in

    object (self)
      val mutable rset = Hashtbl.create 10

      method get_ptr (pa: ptraddr) : bool =
        match pa with
          | UnOP r ->
              Hashtbl.find rset (p_reg' r)
          (* mov (%eax) ,%eax *)
          | BinOP_PLUS (r, _) ->
              Hashtbl.find rset (p_reg' r)
          | BinOP_PLUS_S (r, _) ->
              Hashtbl.find rset (p_reg' r)
          (* mov 0x3c(%esp),%eax *)
          | BinOP_MINUS (r, _) ->
              Hashtbl.find rset (p_reg' r)
          (* mov -0x3c(%esp),%eax *)
          | BinOP_MINUS_S (r , _) ->
              Hashtbl.find rset (p_reg' r)
          (* mov -0x3c(%esp),%eax *)
          | FourOP_PLUS (r1, r2, _, _) ->
              Hashtbl.find rset (p_reg' r1) || Hashtbl.find rset (p_reg' r2)
          (* mov 0x18(%esp,%eax,4),%edx *)
          | FourOP_MINUS (r1, r2, _, _) ->
              Hashtbl.find rset (p_reg' r1) || Hashtbl.find rset (p_reg' r2)
          (* mov -0x18(%esp,%eax,4),%edx *)
          | FourOP_PLUS_S (r1, r2, _, _) ->
              Hashtbl.find rset (p_reg' r1) || Hashtbl.find rset (p_reg' r2)
          (* mov S_0x8048050(%esp,%eax,4),%edx *)
          | FourOP_MINUS_S (r1, r2, _, _) ->
              Hashtbl.find rset (p_reg' r1) || Hashtbl.find rset (p_reg' r2)
          (* mov -S_0x8048050(%esp,%eax,4),%edx *)
          | JmpTable_PLUS (_, r, _) -> (* mov 0x805e17c(,%ebx,4),%eax *)
              Hashtbl.find rset (p_reg' r)
          | JmpTable_MINUS (_, r, _) -> (* lea -4(,%ebx,4),%eax *)
              Hashtbl.find rset (p_reg' r)
          | JmpTable_PLUS_S (_, r, _) -> (* mov S_0x805e17c(,%ebx,4),%eax *)
              Hashtbl.find rset (p_reg' r)
          | JmpTable_MINUS_S (_, r, _) -> (* lea -S_0x805e17c(,%ebx,4),%eax *)
              Hashtbl.find rset (p_reg' r)
          | _ -> failwith "unsupported type in get_ptr"

      method set_ptr (pa: ptraddr) (v : bool) =
        let s = p_ptraddr pa in
          Hashtbl.replace rset s true

    (*get 50 instructions starting from location l *)
    method get_instrs (l : loc) : instr list =
        let count = ref 0 in
          let rec help (il : instr list) (c : int ref) : instr list =
            match (il, !c) with
            | (_, 50) ->  []
            | (h::t, _) ->
              begin
                let l' = get_loc h in
                  if l'.loc_addr >= l.loc_addr then
                    (
                      c := !c + 1;
                      h::(help t c)
                    )
                  else
                    (
                      help t c
                    )
              end
            | ([], _) -> [] in
            help instr_list count

    (*identify
     * mov Const, Reg
     * mov Const, Ptr
    *)
    method is_const_assign i =
      print_string "check const assign\n";
      match i with
        | TripleInstr (p ,e1, e2, _, _) ->
          if is_assign_op p && is_propagation_container e1 && is_const e2 then
            (*should be mov Value, Reg/Ptr*)
            true
          else false
        | _ -> false

    method is_arithmetic i =
      match i with
        | TripleInstr (p ,e1, e2, _, _) ->
          if is_arithmetic_op p && is_propagation_container e1 && is_const e2 then
            (*should be mov Value, Reg/Ptr*)
            true
          else false
        | _ -> false

    method is_propagation (i : instr) : bool =
      match i with
        | TripleInstr (p ,e1, e2, _, _) ->
          if is_assign_op p && is_propagation_container e2 && is_propagation_container e1 then
            (*should be mov Reg/Ptr, Reg/Ptr*)
            true
          else false
        | _ -> false

    method is_pointer_usage i =
    print_string "check pointer usage\n";
      match i with
      | DoubleInstr (p, e, _, _) ->
        begin
          if is_call_jmp p then
            (*
             *  in AT&T syntax assembly code, indirect control flow exp
             *  require * like jmp *%eax
             *)
              match is_star_ptr e with
                | Some e ->
                  begin
                    self#check_control_des e
                  end
                | None -> false
          else
            false
        end
      | TripleInstr (p, e1, e2, _, _) ->
        begin
          if is_assign_op p then
            match e1 with
            | Ptr _ ->
              self#check_container e1
            | _ ->
              begin
                match e2 with
                | Ptr _ ->
                  self#check_container e2
                | _ -> false
              end
          else false
        end
      | _ -> false

    method check_container e =
      match e with
      | Reg r ->
        Hashtbl.find rset (p_reg' r)
      | Ptr pa ->
          self#get_ptr pa
      | _ -> false


    method check_control_des e =
      match e with
      | Reg r ->
        Hashtbl.find rset (p_reg' r)
      | Ptr pa ->
          if self#get_ptr pa == false then
            (*let self = p_ptraddr pa in  *)
              Hashtbl.find rset (p_ptraddr pa)
          else
            true
      | _ -> false

    method elim_target i =
      match i with
      | TripleInstr (p ,e1, e2, _, _) ->
          if is_propagation_container e1 then
            begin
              match e1 with
              | Reg r ->
                Hashtbl.replace rset (p_reg' r) false
              | Ptr pa -> (* we don't support this situation now; probabaly Ptr *)
                Hashtbl.replace rset (p_ptraddr pa) false
              | _ -> failwith "unsupported type in elim_target"
            end
          else failwith "error in elim_target"
      | _ -> failwith "unsupport instruction type in elim_target"

    method propagate i =
      match i with
        | TripleInstr (p ,e1, e2, _, _) ->
          begin
            let set_e1 =
              match e1 with
                | Reg r1 ->
                    Hashtbl.replace rset (p_reg' r1) true
                | Ptr pa1 ->
                    Hashtbl.replace rset (p_ptraddr pa1) true
                | _ -> failwith "unsupported type in elim_target" in
            if is_assign_op p && is_propagation_container e2 && is_propagation_container e1 then
              (*should be mov Reg/Ptr, Reg/Ptr*)
            match e2 with
            | Reg r2 ->
              begin
                if Hashtbl.find rset (p_reg' r2) then  (* this register has the source *)
                  set_e1
                else
                  ()
              end
            | Ptr pa ->
                let s = p_ptraddr pa in
                  if Hashtbl.mem rset s then
                    if Hashtbl.find rset s then
                     (* this (global/stack) memory has been "tainted" *)
                     set_e1
                    else
                      ()
            | _ -> failwith "unsupport exp type in propagate"
          end
        | _ -> failwith "unsupport instruction type in propagate"

(*     method lost_all =
      Hashtbl.fold (fun (k, v, c) ->
                      if c == false then
                        v == true
                      else c
                    ) rset false *)


    method propagate_process (instr_window : instr list) : bool =
      let rec help l =
        match l with
        | h::t ->
          begin
            print_string ((pp_print_instr h)^"\n");
            if self#is_pointer_usage h then
            (
              print_string "detect pointer usage\n";
              true
            )
            else if self#is_const_assign h then
              (
                print_string "detect const assignment\n";
                self#elim_target h;
                help t
              )
            else if self#is_propagation h then
              (
                print_string "propagation detected\n";
                self#propagate h;
                help t
              )
            else if self#is_arithmetic h then
              (
                print_string "arithmetic propagation detected\n";
                help t
              )
            (* else if lost_all i (*then check whether all the containers have lost the source*) *)
              (* false *)
            else
              help t (*if detected, then return, otherwise go on to identify*)
          end
        | [] -> false in (* exceed the maxinum instruction window *)
        help instr_window

    method init_triple_const i =
      match i with
      | TripleInstr (_, e1, _, _, _) ->
        begin
          match e1 with
          | Reg r ->
            Hashtbl.replace rset (p_reg' r) true
          | Ptr pa ->
            self#set_ptr pa true
          | _ -> failwith "unsupported type in init_triple_const"
        end
      | _ -> failwith "error in init_triple_const"


    method df_check_const_val i =
      match i with
        | TripleInstr _ ->
          if self#is_const_assign i then
            let l = get_loc i in
            let h::t = self#get_instrs l in  (* let it crash it not *)
              self#init_triple_const i;
              self#propagate_process t
          else (* we don't support this situation*)
            true
      | DoubleInstr _ ->
        (* possible situation 2 (stack), we don't support this situation*)
        true
      | _ -> true

    method init_containers =
      (* for now, let's just ''taint'' on registers *)
      Hashtbl.replace rset "eax" false;
      Hashtbl.replace rset "ebx" false;
      Hashtbl.replace rset "ecx" false;
      Hashtbl.replace rset "edx" false;
      Hashtbl.replace rset "edi" false;
      Hashtbl.replace rset "esi" false;
      Hashtbl.replace rset "esp" false;
      Hashtbl.replace rset "ebp" false;


    initializer
        self#init_containers;

  end
