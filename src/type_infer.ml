open Type
open Pp_print

open Ail_utils

module TypeInfer = struct

(*
*   type inference module on symbol lifting
* 	Generally, in our type system, we only have two types here
*
*	t ::= num 
		| ref t
*
*
*	Some rules are list here
*
*		E |- r: t E |- r : t 
*		--------------------
*			E |- mov r r : unit

*		E |- n : t E |- r : t
*		-------------------
*			E |- mov n , r : unit
*
*		E |- r1 : ref t1   E |- r2 : t1
*		--------------------------------
*			E |- mov m[r1], r2   : unit
*)

type tyop = Num | Ref

type ty =
  | TypeVariable of int  (* only store the ID of type variable *)
  | TypeOperator of tyop

type term = Propgate | Assign | Pointer | Other
(* different kinds of instructions *)

let next_variable_id = ref 0

let target = ref ""

let make_variable =
  TypeVariable(!next_variable_id)

  let update_env vs value env = 
    if List.mem_assoc vs env then
      (vs,value)::(List.remove_assoc vs env)
    else
      (vs,value)::env

  let update_var vs value var = 
    if List.mem_assoc vs var then 
      (vs,value) :: (List.remove_assoc vs var)
    else
      (vs,value)::var

let init_env_assoc = 
  (*env, associate list*)
  List.fold_left (
                  fun acc r ->
                    let v = make_variable in
                      (r,ref v)::acc
                 ) [] ["eax"; "ebx"; "ecx"; "edx"; "edi"; "esi"; "esp"; "ebp"] ;;

let init_var_assoc = 
  List.fold_left (
                  fun acc r -> 
                      (r,ref false)::acc
                 ) [] ["eax"; "ebx"; "ecx"; "edx"; "edi"; "esi"; "esp"; "ebp"] ;;


  let init_triple_const i env var = 
    match i with
    | TripleInstr (_, e1, _, _, _) ->
      begin
        match e1 with
        | Reg r ->
            let rs = p_reg' r in 
            let var' = update_var rs (ref true) var in 
              env, var'
        | Ptr pa ->
            let ps = p_ptraddr pa in 
            let var' = update_var ps (ref true) var in 
              env, var'
        | _ -> failwith "unsupported instruction type in init_triple_const"
      end
  | _ -> failwith "unsupported instruction type in init_triple_const"


let instr_list = ref None

let set_instr_list (il : instr list) =  
  instr_list := Some (il)
(* 
let get_instr_list : instr list = 
  match !instr_list with 
    | None -> failwith "undefined insr list"
    | Some il -> il 
 *)
(*get 50 instructions starting from location l *)
  let get_instrs il l =
    (* let il = get_instr_list in  *)
      let count = ref 0 in
        let rec help il c =
          match (il, !c) with
          | (_, 50) ->  []
          | (h::t, _) ->
            begin
              let l' = get_loc h in
                if l'.loc_addr > l.loc_addr then
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
          help il count

  let init_env i = 
    let env = init_env_assoc in
    let var = init_var_assoc in
      init_triple_const i env var

  let init_instrlist il i = 
    let l = get_loc i in
      get_instrs il l

  let is_assign_op = function 
    | CommonOP op' ->
      begin
        match op' with
        | Assign _ -> true
        | _ -> false
      end
    | _ -> false

  let is_arithmetic_op = function
    | CommonOP op' ->
      begin
        match op' with
        | Arithm _ -> true
        | _ -> false
      end
    | _ -> false

  let is_const = function
    | Const c -> 
        (
          match c with
          | Normal _ -> true
          | _ -> false
        )
    | _ -> false

  let get_const = function
    | Const c -> 
        (
          match c with
          | Normal n -> n
          | _ -> assert false
        )
    | _ -> assert false

  let is_call_jmp = function
    | ControlOP op -> 
      begin
        match op with
        | Jump _ -> true
        | CALL -> true
        | _ -> false
      end
    | _ -> false

  let is_star_ptr = function
    | Symbol s -> 
      begin
        match s with
        |StarDes e -> Some e
        | _ -> None
      end
    | _ -> None

  let is_reg = function
    | Reg _ -> true (* mov $0x804844b,%eax *)
    | _ -> false

  let is_ptr  = function
    | Ptr _ -> true (* mov $0x804844b,0x14(%esp) *)
    | _ -> false 
      
  let is_propagation_container e =
    (* print_string "in is_propagation_container "; *)
    (* print_exp_type e; *)
    (* print_string "\n"; *)
    is_reg e || is_ptr e 

    (*identify 
     * mov Const, Reg
     * mov Const, Ptr 
    *)
  let is_const_assign i = 
    match i with
      | TripleInstr (p ,e1, e2, _, _) -> 
        if is_assign_op p && is_propagation_container e1 && is_const e2 then 
          (*should be mov Value, Reg/Ptr*)
          true
        else false 
      | _ -> false


  let is_arithmetic i = 
    match i with
      | TripleInstr (p ,e1, e2, _, _) -> 
        if is_arithmetic_op p && is_propagation_container e1 && is_const e2 then 
          (*should be mov Value, Reg/Ptr*)
          true
        else false
      | _ -> false

  let is_propagation i =
    match i with
      | TripleInstr (p ,e1, e2, _, _) -> 
        if is_assign_op p && is_propagation_container e2 && is_propagation_container e1 then 
          (*should be mov Reg/Ptr, Reg/Ptr*)
          true
        else false
      | _ -> false


  let check_container e = 
    match e with
    | Reg r -> 
      true 
    | Ptr pa -> 
      true
    | _ -> false

  let check_control_des e = 
    match e with
    | Reg r -> 
      true
    | Ptr pa -> 
      true
    | _ -> false

  let is_pointer_usage i = 
    match i with
    | DoubleInstr (p, e, _, _) -> 
      begin
        if is_call_jmp p then
          (* 
           *  in AT&T syntax assembly code, indirect control flow exp 
           *  require * like jmp *%eax
           *)
            match is_star_ptr e with (*return exp of star_exp : *(exp) *)
              | Some e' -> 
                  check_control_des e'
              | None -> false
        else
          false
      end
    | TripleInstr (p, e1, e2, _, _) -> 
      begin
        if is_assign_op p then 
          match e1 with
          | Ptr _ -> 
            check_container e1
          | _ -> 
            begin
              match e2 with
              | Ptr _ -> 
                check_container e2
              | _ -> false
            end
        else false
      end
    | _ -> false

  let term_create i =
    if is_propagation i then 
      Propgate 
    else if is_const_assign i then
      Assign 
    else if is_pointer_usage i then 
      Pointer
    else 
      Other

  let new_type es1 env =
   let v = make_variable in
     update_env es1 (ref v) env

  let new_var es1 var =
    update_var es1 (ref false) var

  (* unify value info *)
  let unify_value es1 es2 var = 
    if List.mem_assoc es2 var then 
      let r = List.assoc es2 var in 
        update_var es1 r var
    else
      let var' = new_var es2 var in
      let r = List.assoc es2 var' in 
        update_var es1 r var'
      (* failwith "undefined variable in var assoc list" *)

  (* unify varible type, be more specified, the type variable of es1 will be re-assigned 
     with the type variable of es2
   *)
  let unify_type es1 es2 env = 
    if List.mem_assoc es2 env then 
      let r = List.assoc es2 env in 
        update_env es1 r env 
    else
      let env' = new_type es2 env in 
      let r = List.assoc es2 env' in 
        update_env es1 r env'


  let propagate i env var =
    (* print_instr_type i; *)
    (* print_string "\n"; *)
    (* print_string ((pp_print_instr i)^"\n"); *)
    match i with
      | TripleInstr (p , e1, e2, _, _) ->
        let es1, es2 = 
          match (e1, e2) with
          | (Reg r1, Reg r2) -> 
            (p_reg' r1, p_reg' r2)
          | (Reg r1, Ptr p2) -> 
            (p_reg' r1, p_ptraddr p2)
          | (Ptr p1, Reg r2) -> 
            (p_ptraddr p1, p_reg' r2)
          | (Ptr p1, Ptr p2) -> 
            (p_ptraddr p1, p_ptraddr p2)
          | _ -> failwith "unsupported exp type in propagate" in 
         let env' = unify_type es1 es2 env 
         and var' = unify_value es1 es2 var in 
            env', var'
      | _ -> failwith "unsupported instruction type in propagate"

  let rebind i env var =
    match i with
    | TripleInstr (p ,e1, e2, _, _) -> 
      let es1 = 
        match e1 with
        | Reg r ->
            p_reg' r
        | Ptr pa -> (* we don't support this situation now; probabaly Ptr *)
            p_ptraddr pa
        | _ -> failwith "unsupported type in rebind" in 
        let env' = new_type es1 env 
        and var' = new_var es1 var in 
          env', var' 
    | _ -> failwith "unsupport instruction type in rebind"


      
  (* get the all the variables with same type variable in a list*)
  let get_varl_with_same_tv tv env =
    List.filter (fun (_,tv') -> (!tv) = (!tv')) env

  let pp_var var =
    List.iter (fun (v,t) -> 
          print_string v;
          print_string " ";
          print_string (string_of_bool !t);
          print_string "\n";
          ) var

  let pp_env env =
    List.iter (fun (v,t) -> 
          print_string v;
          (* print_string " "; *)
          (* print_string (string_of_bool !t); *)
          print_string "\n";
          ) env 

  let check_env env var = 
    (* print_string "do check!\n"; *)
    (* pp_var var; *)
    List.exists (
        fun (v,tv') -> 
          if (!tv') = true then  (* this variable has the target value*)
            begin
              (* print_string (v^"\n"); *)
              let temp = List.assoc v env in
              match !temp with
              | (TypeOperator op) -> 
                begin
                  match op with
                  | Ref -> true 
                  | Num -> false
                end
              | _ -> false
            end
          else false
      ) var 


  let pointer_reference env i = 
    (* print_string "do pointer refer : "; *)
    (* print_string (pp_print_instr i); *)
    (* print_string "\n"; *)
    let aux e =
      match is_star_ptr e with
        | Some e' -> 
           begin
              match e' with
              | Ptr p -> Some (p_ptraddr p)
              | Reg r -> Some (p_reg' r)
              | _ -> None
            end
        | None -> None in
    let do_update es env = 
      let tv = List.assoc es env in 
      let var_list = get_varl_with_same_tv tv env in
        List.fold_left (
                fun env' (v, _) -> 
                  update_env es (ref (TypeOperator Ref)) env' 
                  ) env var_list in
    match i with
    | DoubleInstr (p, e, _, _) -> 
      begin
        if is_call_jmp p then
          (* 
           *  in AT&T syntax assembly code, indirect control flow exp 
           *  require * like jmp *%eax
           *)
          begin
            match aux e with
              | Some es -> do_update es env 
              | None -> failwith "unsupported type in pointer_reference"
          end
        else
          failwith "unsupported type in pointer_reference"
      end
    | TripleInstr (p, e1, e2, _, _) -> 
      begin
        if is_assign_op p then 
          let e1' = aux e1 
          and e2' = aux e2 in
            match (e1', e2') with
            | (Some es1, Some es2) -> 
              (* we found two pointers, is it possible ?*)
              let env' = do_update es1 env in 
                do_update es2 env'
            | (Some es1, None) -> 
              do_update es1 env 
            | (None, Some es2) -> 
              do_update es2 env 
            | (None, None) -> failwith "unsupported type in pointer_reference"
        else
          failwith "unsupported type in pointer_reference"
      end
    | _ -> failwith "unsupported type in pointer_reference"


  let get_target_addr i = 
    match i with
      | TripleInstr (p ,e1, e2, _, _) -> 
        if is_assign_op p && is_propagation_container e1 && is_const e2 then 
          dec_hex (get_const e2)
        else assert false
      | _ -> assert false

(* a "fold_until" function *)
  let rec analyse (env, var) il = 
    match il with
      | i::tl -> 
        begin
          (* print_string ((pp_print_instr i)^"\n"); *)
          match term_create i with
            | Propgate -> 
                analyse (propagate i env var) tl
            | Assign -> 
                analyse (rebind i env var) tl
            | Pointer -> 
              let env' = pointer_reference env i in 
                begin
                 match check_env env' var with 
                 | true -> (print_string ("\n    value "^(!target)^" can be inferred as a label!\n"); true)
                 | false -> 
                    analyse (propagate i env var) tl
                end
            | Other -> analyse (env, var) tl
          end
      | [] -> (print_string "\n    no judgement after analysis 50 instructions \n"; true)
  
  (* do type infer on instruction  *)
  let type_infer (il : instr list) (i : instr) : bool = 
    if is_const_assign i then (
      target := get_target_addr i;
      let (env, var) = init_env i in
      let il' = init_instrlist il i in
        analyse (env, var) il'
    )
    else (print_string "\n    instruction type is not supported\n"; true)

end ;;
