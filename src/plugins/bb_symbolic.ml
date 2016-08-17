(* this module leveages the symbolic execution engine *)
(* (symbolic_engine/engine.py) to execute instructions. note that we only feed *)
(* our engine with instructions from each basic block *)


(* this module should make some preprossing and post-processing *)

(* 1. preprocess the instructions in each basic block, we might need to filter *)
(* some instructions out (remember we don't keep track of control flow transfers) *)

(* 2. pp print instructions*)

(* post-processing? we currently keep track of eight registers; to see what is *)
(* the reasults. *)



module BB_symbolic = struct


    open Ail_utils
    open Pp_print_intel
    open Type
    open Lea_parser
    open Mem_parser
    open Call_parser


    let call_ret_process il =
      let module CP = Call_parser in
      let rec aux il =
        let trans i =
        match get_op i with
        | ControlOP (CALL) | ControlOP (CALLQ)
        | ControlOP (RET) | ControlOP (RETN) | ControlOP (RETQ)
          -> CP.process i
        | _ -> [i]
        in
        match il with
          | [] -> []
          | (h::t) ->
             let il = trans h in
             il :: (aux t)
      in
      aux il
      |> List.flatten



    let lea_process il =
      let module LP = Lea_parser in
      let rec aux il =
        let trans i =
        match get_op i with
          | CommonOP (Assign LEA) ->
             LP.process i
          | _ -> [i]
        in
        match il with
          | [] -> []
          | (h::t) ->
             let il = trans h in
             il :: (aux t)
      in
      aux il
      |> List.flatten


    (* parse memory experand  *)
    let mem_process il =
      let module MP = Mem_parser in
      let rec aux il =
        let trans i =
          MP.process i
        in
        match il with
          | [] -> []
          | (h::t) ->
             let il = trans h in
             (* TODO: change to tail recursive *)
             il :: (aux t)
      in
      aux il
      |> List.flatten


    let stack_op_process il =
      let open Lifter in
      let module OU = Opcode_utils in
      let module SC = Stack_Change in
      let rec aux il =
        let trans i =
        match (OU.is_stack_op @@ get_op i) with
          | true -> SC.stack i
          | false -> [i]
        in
        match il with
          | [] -> []
          | (h::t) ->
             let il = trans h in
             il :: (aux t)
      in
      aux il
      |> List.flatten


    let load_libc_func () =
      let update ll =
        let aux l =
          Printf.sprintf "S_0x%s" @@ String.uppercase @@ String.sub l 1 7
        in
        List.map aux ll
      in
      (* this function load libc attched function information, as they are
      excluded in funcID project  *)
	(*
      let black_list = ["<__libc_csu_init>:"; "<__libc_csu_fini>:";
                        "<__i686.get_pc_thunk.bx>:";
                        "<__x86.get_pc_thunk.bx>:";
                        "<__do_global_ctors_aux>:";
                        "<_start>:";
                        "<atexit>:";
                        "<__do_global_dtors_aux>:"; "<frame_dummy>:"]
	*)
      let black_list = []
      in
      let aux r l =
        let open Utils in
        let items = str_split_by_space l in
        if List.mem (List.nth items 1) black_list then
          (* found a target *)
          (List.nth items 0) :: r
        else
          r
      in
      let fl = read_file "faddr_old.txt" in
      List.fold_left aux [] fl
      |> update


    (* obtain the bmap *)
    let construct_bmap il bbl =
      let black_list = load_libc_func () in
      let filter_libc_func bbl =
        let m = Hashtbl.create 50 in
        let aux b il =
          if List.mem b.bf_name black_list then
            begin
              (*   print_endline @@ b.bf_name; *)
              (* FIXME: using code below can reduce the recall; so wired *)
              (* Hashtbl.replace m b.bblock_name [] *)
              ()
            end
          else
            Hashtbl.replace m b.bblock_name il
        in
        Hashtbl.iter aux bbl;
        m
      in
      let module BU = BB_utils in
     (* let bmap = BU.bb_map bbl il in *)
      BU.bb_map_with_b bbl il
	  (* 2015-12-12: note that here, we don't dump libc attached functions *)
      |> filter_libc_func
      (*
      let aux k v =
        print_endline k;
        print_int @@ List.length v;
        print_endline @@ pp_print_instr @@ List.nth v 0;
        print_newline;
        ()
      in
      Hashtbl.iter aux bmap
      *)

    (* preprocess a instruction list  *)
    let preprocess il =
      let str i =
        let p_triple p e1 e2 =
          let p_str = p_op p
          and e1_str = p_exp e1
          and e2_str = p_exp e2 in
            (p_str^" "^e2_str^" "^e1_str)
        in
        let p_four p e1 e2 e3 =
          let p_str = p_op p
          and e1_str = p_exp e1
          and e2_str = p_exp e2
          and e3_str = p_exp e3 in
            (p_str^" "^e3_str^" "^e2_str^" "^e1_str)
        in
        match i with
        | SingleInstr (p, l, pre) -> p_single p
        | DoubleInstr (p, exp1, l, pre) -> p_double p exp1
        | TripleInstr (p, exp1, exp2, l, pre) -> p_triple p exp1 exp2
        | FourInstr (p, exp1, exp2, exp3, l, pre) -> p_four p exp1 exp2 exp3
      in
      let filter i =
        let module OU = Opcode_utils in
        (* we don't consider intra and inter-procedure control transfer *)
        get_op i
        |> (fun o ->
            OU.is_control_transfer_op o || OU.is_cmp_op o
           ) = false
      in
      il
      |> List.filter filter
      |> lea_process
      |> call_ret_process
      |> stack_op_process
      |> mem_process
      |> List.map str


    (* dump instruction list into a file  *)
    let dump bn il =
      let module UIO = UIO in
      bn
      |> (fun n -> "instr_list_" ^ n ^ ".txt")
      |> UIO.write_file il



    let execute bmap =
      let aux bn il =
        dump bn @@ preprocess il;
        (* this method is too slow.. I want to use some multiple thread methods. *)
	(* abondam *)
        (*  Sys.command("python plugins/symbolic_engine/engine.py " ^ bn); *)
        ()
      in
      Hashtbl.iter aux bmap;
      ()


    let process instrs bbl =
      construct_bmap instrs bbl |> execute


 end
