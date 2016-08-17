open Batteries

open Printf
open Visit
open Type
open Ail_utils

open Dataflow_analysis
open Type_infer
open Pp_print

open Share_lib_helper
open Data_process


exception Reassemble_Error of string;;

let rev_map f l =
  let rec aux f l acc =
    match l with
    | h::t -> aux f t ((f h)::acc)
    | [] -> acc in
  f l []


class datahandler (label' : (string*int) list) =
object (self)
  val mutable sec : section list = []
  val mutable data : string list = []
  val mutable rodata : string list = []
  val mutable got : string list = []
  val mutable bss : string list = []

  val mutable text_mem_addrs: string list = []

  val mutable data_labels : (string*int) list = []
  val mutable text_labels : int list = []

  val mutable label : (string*int) list = label'
  val mutable label_set : (int) list = []

  val mutable data_list: (string*string) list = []
  val mutable rodata_list: (string*string) list = []
  val mutable got_list: (string*string) list = []
  val mutable bss_list: (string*string) list = []

  val mutable data_array: (string*string) array = [||]
  val mutable rodata_array: (string*string) array = [||]
  val mutable got_array: (string*string) array = [||]
  val mutable bss_array: (string*string) array = [||]

  val mutable text_sec: (int*int) = (0,0)  (* begin addr, size*)
  val mutable locations = []

  val mutable in_jmptable = false

  method set_datas funcs =
    self#section_collect;
    self#data_collect;

    data_list <- self#data_trans data;
    rodata_list <- self#data_trans rodata;
    got_list <- self#data_trans got;
    bss_list <- self#data_trans bss;
    locations <- self#label_locate;

    label_set <- List.map snd label;

    text_mem_addrs <- List.map (String.trim) (read_file "text_mem.info");

    (*
    FIXME : this method use a trivial way to solve references in data
    sections, the identified references could be in text sections or data
    sections, we might only need references towards the text sections, and do
    not resolve data labels any more.
     *)
    self#data_refer_solve funcs


  method set_datas_1 =
    self#section_collect;
    self#data_collect;

    data_list <- self#data_trans data;
    rodata_list <- self#data_trans rodata;
    got_list <- self#data_trans got;
    bss_list <- self#data_trans bss;
    locations <- self#label_locate;

    label_set <- List.map snd label;

    text_mem_addrs <- List.map (String.trim) (read_file "text_mem.info");

    (*
    FIXME : this method use a trivial way to solve references in data
    sections, the identified references could be in text sections or data
    sections, we might only need references towards the text sections, and do
    not resolve data labels any more.
     *)
    self#data_refer_solve1


  method get_textlabel =
    text_labels

  method text_sec_collect =
    let filelines = File.lines_of "text_sec.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3)) in
      text_sec <- (addr, size)
    in
    Enum.iter help filelines


  (* bug found : even if the suspecious address in inside text section's range, as long as
   *    it is not in the virtual memory address list, we should NOT consider it as a memory address!!
   ********* bugs are found in PerlBench ****************
   *)

  method check_text addr =
    let dec_hex n =
  	  (Printf.sprintf "%x:" n) in
    let judge_mem_list addr =
	  let addrs = dec_hex addr in
	  List.mem addrs text_mem_addrs in
    let (b,size) = text_sec in
    let e = b + size in
    if addr = 0xffff then false
    (* this is dangerous, when processing 64-bit shared object, for example,
    ssh, the text section range includes memory address 0xffff. However, I
    found instructions like this in the code:
     cmp $0xffff,%eax
     *)
    else if (addr>=b && addr < e) && (judge_mem_list addr) then true
    else false

  (* this method solve all the references in the .data .rodata sections *)
  method data_refer_solve funcs =
    let begin_addrs = List.map (fun f -> f.func_begin_addr) funcs in
    let check_func_begin v =
      List.mem v begin_addrs in
    (* this method traverse data list, check each four byte value *)
    let get_v s =
      let items = Str.split (Str.regexp " +") s in
      let v = List.nth items 1 in
      String.sub v 2 ((String.length v)-2) in
    let rec traverse acc l =
      (*
               There exists a bug in 64 bit ELF processing. The address
               produced in "let vs'" line contains a 0 at the beginning.
               It musted be removed though.

               aux function translate value_str to value, then to value_str' ;
             thus getting rid of capital zeros.
       *)
      let aux vs =
        let v = int_of_string vs in
        (Printf.sprintf "%X" v) in
      match l with
      | [] -> acc
      | h::[] -> h::acc
      | h1::h2::[] -> h2::h1::acc
      | h1::h2::h3::[] -> h3::h2::h1::acc
      | (l1,v1)::(h2,v2)::(l3,v3)::(l4,v4)::t ->
         (
           let v_str = "0x"^(get_v v4)^(get_v v3)^(get_v v2)^(get_v v1) in
           match string_to_int32 v_str with
           | Some v ->
              begin
                match (self#check_sec v) with
                | Some s ->
                   (
                     (*
                                            let v_str' = String.sub v_str 4
                                              ((String.length v_str)-4) in
                      *)
                     let v_str' = aux v_str in
                     data_labels <- (s.sec_name, v)::data_labels;

                     (traverse (("","")::("","")::("","")::(l1,".long S_0x"^(String.uppercase v_str'))::acc)
                               t)

			(*

                     (* we don't resolve references inside data sections *)
                     begin
                       in_jmptable <- false;
                       (*  let h::t = l in (* let it crash if not pattern matched *)
                       traverse ((l1,v1)::acc) t
                        *)
                       traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                     end
			*)
                   )
                | None ->
                   (
                     match (self#check_text v) with
                     | true ->
                        (
                          (*
                                              let v_str' = String.sub v_str 3
                                              ((String.length v_str)-3) in
                           *)
                          (* if it is a begin addr of a function *)
                          if (check_func_begin v) = false then
                            begin
                              if (self#check_jmptable l1) = false then
                                begin
                                  in_jmptable <- false;
                                  (* let h::t = l in
                                  traverse ((l1,v1)::acc) t
                                   *)
                       		      traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                                end
                              else
                                begin
                                  in_jmptable <- true;
                                  let v_str' = aux v_str in
                                  text_labels <- v::text_labels;
                                  (traverse (("","")::("","")::("","")
                                             ::(l1,".long S_0x"^(String.uppercase
                                                                   v_str'))::acc) t)
                                end
                            end
                          else
                            begin
                              (* FIXME: some of the jump tables might contains both
                     "function pointer" or destinations...
                               *)
                              (*      in_jmptable <- false; *)
                              in_jmptable <- true;
                              let v_str' = aux v_str in
                              text_labels <- v::text_labels;
                              (traverse (("","")::("","")::("","")
                                         ::(l1,".long S_0x"^(String.uppercase
                                                               v_str'))::acc) t)
                            end
                        )
                     | false ->
                        begin
                          in_jmptable <- false;
                          (* let h::t = l in
                          traverse ((l1,v1)::acc) t
                           *)
                       	  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                        end
                   )
              end
           | None ->
              begin
                in_jmptable <- false;
                (*
                let h::t = l in  (*let it crash it not matched*)
                traverse ((l1,v1)::acc) t
                 *)
                traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
              end
         ) in

    (* add labels in data, rodata sections to support check jmp table*)
    self#add_data_label;
    data_list <- List.rev (traverse [] data_list);
    rodata_list <- List.rev (traverse [] rodata_list);
    got_list <- List.rev (traverse [] got_list)


  method data_refer_solve1 =
    let get_v s =
      let items = Str.split (Str.regexp " +") s in
      let v = List.nth items 1 in
      String.sub v 2 ((String.length v)-2) in
    let rec traverse acc l =
      (*
               There exists a bug in 64 bit ELF processing. The address
               produced in "let vs'" line contains a 0 at the beginning.
               It musted be removed though.

               aux function translate value_str to value, then to value_str' ;
             thus getting rid of capital zeros.
       *)
      let aux vs =
        let v = int_of_string vs in
        (Printf.sprintf "%X" v) in
      match l with
      | [] -> acc
      | h::[] -> h::acc
      | h1::h2::[] -> h2::h1::acc
      | h1::h2::h3::[] -> h3::h2::h1::acc
      | (l1,v1)::(h2,v2)::(l3,v3)::(l4,v4)::t ->
         (
           let v_str = "0x"^(get_v v4)^(get_v v3)^(get_v v2)^(get_v v1) in
           match string_to_int32 v_str with
           | Some v ->
              begin
                match (self#check_sec v) with
                | Some s ->
                   (
                     begin
                       in_jmptable <- false;
                       traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                     end
                   )
                | None ->
                   (
                     match (self#check_text v) with
                     | true ->
                        (
                          if (self#check_jmptable l1) = false then
                            begin
                              in_jmptable <- false;
                       		  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                            end
                          else
                            begin
                              in_jmptable <- true;
                              let v_str' = aux v_str in
                              text_labels <- v::text_labels;
                              (traverse (("","")::("","")::("","")
                                         ::(l1,".long S_0x"^(String.uppercase
                                                               v_str'))::acc) t)
                            end
                        )
                     | false ->
                        begin
                          in_jmptable <- false;
                       	  traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
                        end
                   )
              end
           | None ->
              begin
                in_jmptable <- false;
                traverse ((l4,v4)::(l3,v3)::(h2,v2)::(l1,v1)::acc) t
              end
         ) in

    self#add_data_label;
    data_list <- List.rev (traverse [] data_list);
    rodata_list <- List.rev (traverse [] rodata_list);



  method check_jmptable addrs =
    (* our judgement of jmptable in rodata section *)
    (*
        1. begin with a label (probably indicates a valid des?)
        2. last time we have defined a label (no byte value embedded)
        3. all these labels are within one function (how to check it?)
     *)
    if in_jmptable = true then
      true
    else
      begin
        try
          let addr = int_of_string addrs in
          if List.mem addr label_set then
            begin
              in_jmptable <- true;
              true
            end
          else
            false
        with _ -> false
      end


  method pp_print l =
    let rec help l =
      match l with
      | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
      | [] -> print_string "end\n" in
    help l

  method section_collect =
    let filelines = File.lines_of "sections.info"
    and help l =
      let items = Str.split (Str.regexp " +") l in
      let addr = int_of_string ("0x"^(List.nth items 1))
      and size = int_of_string ("0x"^(List.nth items 3))
      and secname = List.nth items 0 in
      sec <- {sec_name=secname; sec_begin_addr=addr;
              sec_size=size}::sec
    in
    Enum.iter help filelines

  method section_offset name addr =
    let rec help l =
      match l with
      | h::t -> if h.sec_name = name then addr - h.sec_begin_addr
                else help t
      | [] -> raise (Reassemble_Error "fail in section offset") in
    help sec

  method section_addr name =
    let rec help l =
      match l with
      | h::t -> if h.sec_name = name then h.sec_begin_addr
                else help t
      | [] -> raise (Reassemble_Error "fail in section addr") in
    help sec

  method data_collect =
    Sys.command("python spliter.py");
    (*print_string "begin collection\n"; *)
    data <- self#collect "data_split.info";
    (*print_string "finish data\n"; *)
    rodata <- self#collect "rodata_split.info";
    (*print_string "finish rodata\n"; *)
    got <- self#collect "got_split.info";
    (*print_string "finish got\n"; *)
    bss <- self#collect_bss "bss.info";
  (*print_string "finish collection\n"*)

  method sec_transform s =
    match s with
    | ".rodata" -> ".rodata"
    | ".got" -> ".got"
    | "bss" -> ".bss"
    | _ -> ".rodata"

  method check_sec addr =
    let rec help l addr =
      match l with
      | h::t -> (
        let b = h.sec_begin_addr in
        let e = b + h.sec_size in
        if (addr>=b && addr < e) then
          Some (h)
        else  help t addr )
      | [] -> None in
    help sec addr

  method pp_print_2 l =
    let rec help l =
      match l with
      | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
      | [] -> print_string "end\n" in
    help l

  method pp_print_1 l =
    let rec help l =
      match l with
      | h::t -> (printf "item: %s ") h; help t
      | [] -> print_string "end\n" in
    help l

  method data_transform data_str =
    let l = String.length data_str in
    if l mod 2 <> 0 then failwith "data collection error\n"
    else
      let p = object
          val mutable x: (string*string) list = []
          method get_list = x
          method set_list i = x <- ("",".byte 0x"^i)::x
        end
      and help s =
        let rec help' i l =
          if i < 0 then l else help'(i-2) ((Char.escaped(s.[i-1])^Char.escaped(s.[i]))::l) in
        help' (String.length s - 1) [] in
      let data_list = List.rev (help data_str) in
      let () = List.iter p#set_list data_list in
      p#get_list

  method data_trans data_list =
    let p = object
        val mutable x: (string*string) list = []
        method get_list = x
        method set_list i = x <- ("",i)::x
      end in
    List.iter p#set_list data_list;
    p#get_list


  method data_bss data_list =
    let p = object
        val mutable x: (string*string) list = []
        method get_list = x
        method set_list i = x <- ("",i)::x
      end in
    List.iter p#set_list data_list;
    p#get_list


  method label_locate =
    let rec help l acc =
      match l with
      | (s,l)::t -> let offset = self#section_offset s l in
                    (s,offset)::acc
      | [] -> acc in
    List.fold_left help label []

  method add_data_label =
    let p = object(sp)
              method process lbs =
                let dec_hex (s:int) : string =
                  (Printf.sprintf "0x%X" s) in
                let rec help loc_list =
                  match loc_list with
                  | (n, l)::t -> begin
                      match n with
                      | ".data" ->
                         ( let off = l - (self#section_addr ".data") in
                           let s' = dec_hex l
                           and (s,d) = data_array.(off) in
                           data_array.(off) <- (s', d);
                           help t )
                      | ".rodata" ->
                         ( let off = l - (self#section_addr ".rodata") in
                           let s' = dec_hex l
                           and (s,d) = rodata_array.(off) in
                           rodata_array.(off) <- (s', d);
                           help t )
                      | _ -> help t
                    end
                  | _ -> ()
                in
                help lbs
            end in
    rodata_array <- Array.of_list rodata_list;
    data_array <- Array.of_list data_list;
    p#process locations;
    rodata_list <- Array.to_list rodata_array;
    data_list <- Array.to_list data_array;

  method data_output =
    let p = object(sp)
              method process lbs =
                let dec_hex (s:int) : string =
                  (Printf.sprintf "S_0x%X : " s) in
                let rec help loc_list =
                  match loc_list with
                  | (n, l)::t -> begin
                      match n with
                      | ".data" ->
                         ( let off = l - (self#section_addr ".data") in
                           let s' = dec_hex l
                           and (s,d) = data_array.(off) in
                           data_array.(off) <- (s', d);
                           help t )
                      | ".rodata" ->
                         ( let off = l - (self#section_addr ".rodata") in
                           let s' = dec_hex l
                           and (s,d) = rodata_array.(off) in
                           rodata_array.(off) <- (s', d);
                           help t )
                      | ".got" ->
                         ( let off = l - (self#section_addr ".got") in
                           let s' = dec_hex l
                           and (s,d) = got_array.(off) in
                           got_array.(off) <- (s', d);
                           help t )
                      | ".bss" ->
                         ( let off = l - (self#section_addr ".bss") in
                           let s' = dec_hex l
                           and (s,d) = bss_array.(off) in
                           bss_array.(off) <- (s', d);
                           help t )
                      | _ -> raise (Reassemble_Error n)
                    end
                  | _ -> ()
                in
                help lbs
              method zip ll =
                List.map (fun (h,e) -> h^e) ll
              method zip2 ll =
                List.map (fun (h,e) -> h^" "^(string_of_int e)^"\n") ll
              method insert_dummy =
                let help = function
                  | (l,s)::t -> ("s_dummy: \n"^l, s)::t
                  | _ -> failwith "empty rodata list" in
                rodata_list <- help rodata_list
              method insert_head =
                let rec sublist b e l =
                  match l with
                    [] -> failwith "sublist"
                  | h :: t ->
                     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
                     if b>0 then tail else h :: tail in
                rodata_list <- (".section .rodata\n", "")::rodata_list;
                got_list <- (".section .got\n", "")::got_list;
                data_list <- (".section .data\n", "")::data_list;
                bss_list <- (".section .bss\n", "")::bss_list
              method write_file =
                let oc = open_out_gen [Open_append; Open_creat] 0o666 "final_data.s" in
                (*
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip rodata_list);
                (* List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip *)
                (* got_list); *)
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip data_list);
                List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip bss_list);
                 *)
                Printf.fprintf oc "%s\n" (String.concat "\n" (sp#zip rodata_list));
                (* List.iter (fun l -> Printf.fprintf oc "%s\n" l) (sp#zip *)
                (* got_list); *)
                Printf.fprintf oc "%s\n" (String.concat "\n" (sp#zip data_list));
                Printf.fprintf oc "%s\n" (String.concat "\n" (sp#zip bss_list));
                close_out oc
              method de_redunt labels =
                let tset = Hashtbl.create 200 in
                let help item =
                  let (n, l) = item in
                  if Hashtbl.mem tset l then
                    ()
                  else
                    Hashtbl.replace tset l item;
                  () in
                List.iter help labels;
                let help' key value ll =
                  value::ll in
                Hashtbl.fold help' tset []
            end in
    (* process text and process data are two different approaches
     * process text resolve all the references from text section,
     * process data resolve interleave reference among rodata and data sections
     * current we just leverage a heristic methods, which consider all the value
     * inside .data and .rodata sections as addr *)
    (* print_int (List.length bss_list); *)
    (* let temp = p#zip2 locations in *)
    (* List.iter print_string temp; *)
    (* print_int (List.length bss_list); *)
    rodata_array <- Array.of_list rodata_list;
    got_array <- Array.of_list got_list;
    data_array <- Array.of_list data_list;
    bss_array <- Array.of_list bss_list;
    (* data_labels <- p#de_redunt data_labels; *)
    p#process locations;
    p#process data_labels;
    rodata_list <- Array.to_list rodata_array;
    got_list <- Array.to_list got_array;
    data_list <- Array.to_list data_array;
    bss_list <- Array.to_list bss_array;
    p#insert_dummy;
    p#insert_head;
    p#write_file
  (*
        method update_list li loc addr =
            let dec_hex (s:int) : string =
              "S_0x"^(Printf.sprintf "%X : " s) in
            let rec help l n acc =
                match (l,n) with
                | ((s,d)::t , 0) ->
                  let s' = dec_hex loc in
                                        acc@[(s',d)]@t
                | (h::t, n) -> help t (n-1) (acc@[h])
                | ([],_) ->
                    raise (Reassemble_Error "error in update list")
                | ([h], n) when n > 0 ->
                    raise (Reassemble_Error "error in update list") in
                let off = loc - addr in
                  if off < 0 then raise (Reassemble_Error "error in update list")
                  else help li off []
   *)
  method collect_ocaml name =
    if Sys.file_exists(name) then begin
        let filelines = File.lines_of name
        and p = object
            val mutable c : string = ""
            method process line =
              let line' = String.trim line in
              let rev l =
                let s1 = (Char.escaped(l.[0])^Char.escaped(l.[1]))
                and s2 = (Char.escaped(l.[2])^Char.escaped(l.[3]))
                and s3 = (Char.escaped(l.[4])^Char.escaped(l.[5]))
                and s4 = (Char.escaped(l.[6])^Char.escaped(l.[7])) in
                s4^s3^s2^s1 in
              let items = Str.split (Str.regexp " +") line' in
              c <-
                c^(List.fold_left (fun acc item -> acc^(item))
                                  "" items)
            method get_c = c
          end  in
        Enum.iter p#process filelines;
        p#get_c
      end
    else ""


  (* let's try python script *)
  method collect name =
    if Sys.file_exists(name) then begin
        let lines = read_file name in
	List.map String.trim lines
	end
   else
       []

  (* this is a optimizated solution for large size .bss section ELF binary *)
  method collect_bss name =
    if Sys.file_exists(name) then begin
        let filelines = File.lines_of name
        and p = object
            val mutable c : string list = []
            method process line =
              let line' = String.trim line in
              c <- line'::c;
            method get_c = c
          end
        in
        Enum.iter p#process filelines;
        p#get_c
      end
    else [""]

end


class instrhandler instr_list des'  =
(* this class handle control flow transfer, change concrete addr in the .text section
* into symbols
* WATCH OUT:
*        basically as we use objdump to disassmble the binaries, then currently we can only
*        handle control flow transfer destination which is provided by objdump
*        Then if destination is inside one instruction (not one opcode!!), we cannot handle it!
*        Exceptions will be thrown in that case.
*)
    object (self)
        val mutable des: string list = des'
        val mutable locs : loc list = []
        val mutable new_instrlist = []

        method print_loclist =
         let dec_hex (s:int) : string =
              "S_0x"^(Printf.sprintf "%X" s) in
          let help l =
            let d = dec_hex l.loc_addr in
            print_string (l.loc_label^" : "^d^"\n") in
            List.iter help locs

        method get_instr_list =
          let rec help acc i_list l_list =
            match (i_list, l_list) with
            | (i::ti, h::tl) ->  help ((set_loc i h)::acc) ti tl
            | ([], []) -> List.rev acc
            |_ -> raise (Reassemble_Error "instrhandler error in get_instr_list") in
          help [] instr_list locs

        method set_instr_list =
          let rec help acc l =
            match l with
            | h::t -> help ((get_loc h)::acc) t
            | [] -> List.rev acc in
          locs <- help [] instr_list;
          (* locs <- List.rev locs *)
          (* self#print_loclist *)

        method pp_print l =
          let rec help l =
            match l with
            | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
            | [] -> print_string "end\n" in
            help l

        method pp_print1 l =
          let rec help l =
            match l with
            (*| h1::t ->  print_string ((string_of_int h1.loc_addr)^"\n"); help t *)
            | h1::t ->  print_string (h1^"\n"); help t
            | [] -> print_string "end\n" in
            help l

        method pp_print2 l =
          let rec help l =
            match l with
            | h1::t ->  print_string ((string_of_int h1.loc_addr)^"\n"); help t
            | [] -> print_string "end\n" in
            help l

      	method clean_sort ll =
      	  let ll' = List.map( fun l ->
      		    if String.contains l '$' then
      			String.sub l 1 (String.length l - 1)
      		    else l
      			 ) ll in
          let ll'' = List.map (fun l -> String.sub l 2 (String.length l - 2)) ll' in
          let lld = List.map (int_of_string) ll'' in
          let lld' = List.filter (fun l -> l<>0) lld in
      	  let lld1 = unify_int_list lld' in
          let lld2 = List.sort Int.compare lld1 in
          List.sort Int.compare lld1
         (* let lld2 = List.sort Int.compare lld1 in
         List.map (Printf.sprintf "S_0x%X") lld2 *)

        method process =
          let dec_hex (s:int) : string =
              (Printf.sprintf "S_0x%X" s) in
          let do_update s n =
              if String.exists s n then s
              else s^"\n"^n in
          let rec help acc llist dlist =
            match (llist,dlist) with
              | (llist',[]) -> List.rev_append llist' acc
              | (lh::lt,dh::dt) ->
                 (*
            		let lhs = dec_hex lh.loc_addr in
            		  if String.exists dh lhs then
                    (
                      let label' = do_update lh.loc_label (lhs^" : ") in
                        let lh' = {lh with loc_label=label'} in
                          help (lh'::acc) lt dt
                    )
            		  else if (String.compare dh lhs) < 0 then
                    (* this is not a label indeed*)
            			  help (lh::acc) lt dt
            		  else
            		    help (lh::acc) lt dlist
                  *)
            	 let lhd = lh.loc_addr in
            	 if dh = lhd then
                   (
            		 let lhs = dec_hex lh.loc_addr in
                     let label' = do_update lh.loc_label (lhs^" : ") in
                     let lh' = {lh with loc_label=label'} in
                     help (lh'::acc) lt dt
                   )
            	 else if dh < lhd  then
                   (* this is not a label indeed*)
            	   help (lh::acc) lt dt
            	 else
            	   help (lh::acc) lt dlist
      	      | (a,b) ->
            	 (
                   print_string "reassemble process line 535\n";
            	   print_string ((dec_hex (List.nth b 0))^"\n");
            	   print_string ((string_of_int (List.length a))^"\n");
            	   print_string ((string_of_int (List.length b))^"\n");
            	   failwith "undefined des list"
            	 )
          in
      	  let des' = self#clean_sort des in
      	  locs <- List.rev (help [] locs des')

        method insert_dummy =
          let help = function
            | h::t -> {h with loc_label=".globl main\n"^h.loc_label}::t
            | _ -> failwith "empty loc list" in
          locs <- help locs

        method update_loc locs d =
          let dec_hex (s:int) : string =
            "S_0x"^(Printf.sprintf "%X : " s) in
          let identify_des addr1 addr2 =
            addr1.loc_addr = (int_of_string addr2)
          and lift_addr addr =
            dec_hex addr
          and do_update s n =
            if String.exists s n then s
            else s^"\n"^n in
          let rec help d l =
            if identify_des l d then
              {l with loc_label=(do_update l.loc_label (lift_addr l.loc_addr))}
            else l in
          let items = Str.split (Str.regexp "_") d in
          let d' = List.nth items 1 in
          List.map (help d') locs

    end

class funchandler instr_list u_funcs'  =
(* this class handle control flow transfer, change concrete addr in the .text section
* into symbols
* WATCH OUT:
*        basically as we use objdump to disassmble the binaries, then currently we can only
*        handle control flow transfer destination which is provided by objdump
*        Then if destination is inside one instruction (not one opcode!!), we cannot handle it!
*        Exceptions will be thrown in that case.
*)
    object (self)
        val mutable funcs: func list = u_funcs'
        val mutable locs : loc list = []
        val mutable new_instrlist = []

        method print_loclist =
         let dec_hex (s:int) : string =
              "S_0x"^(Printf.sprintf "%X" s) in
          let help l =
            let d = dec_hex l.loc_addr in
            print_string (l.loc_label^" : "^d^"\n") in
            List.iter help locs


        method get_instr_list =
          let rec help acc i_list l_list =
            match (i_list, l_list) with
            | (i::ti, h::tl) -> help ((set_loc i h)::acc) ti tl
            | ([], []) -> List.rev acc
            |_ -> raise (Reassemble_Error "funchandler error in get_instr_list") in
          help [] instr_list locs

        method set_instr_list =
          let rec help acc l =
            match l with
            | h::t -> help ((get_loc h)::acc) t
            | [] -> List.rev acc in
          locs <- help [] instr_list;
          (* self#print_loclist *)


        method pp_print l =
          let rec help l =
            match l with
            | (h1,h2)::t -> (printf "item: (%s,%d) ") h1 h2; help t
            | [] -> print_string "end\n" in
            help l

        method process =
          let rec help dlist =
            match dlist with
              | d::t ->  (locs <- self#update_loc locs d; help t)
              | _ -> ()
              in help funcs

        method func_sort ll =
            List.sort (fun f1 f2 -> f1.func_begin_addr - f2.func_begin_addr) ll

        method process2 =
	(*
          let dec_hex (s:int) : string =
              (Printf.sprintf "S_0x%X" s) in
          let do_update s n =
              if String.exists s n then s
              else s^"\n"^n in
	*)
          let rec help acc llist flist =
            match (llist,flist) with
              | (llist',[]) -> List.rev_append llist' acc
              | (lh::lt,fh::ft) ->
                  if lh.loc_addr == fh.func_begin_addr then
                    (
                      let lh' = {lh with loc_label=(fh.func_name^" :\n"^lh.loc_label)} in
                          help (lh'::acc) lt ft
                    )
                  else if lh.loc_addr > fh.func_begin_addr then
                    help (lh::acc) lt ft
                  else
                    help (lh::acc) lt flist
              | (a,b) ->
                (
                  (* print_string ((List.nth b 0)^"\n"); *)
                  print_string ((string_of_int (List.length a))^"\n");
                  print_string ((string_of_int (List.length b))^"\n");
                  failwith "undefined des list"
                )
              in
          let funcs' = self#func_sort funcs in
            locs <- List.rev (help [] locs funcs')

         method insert_dummy =
            let help = function
            | h::t -> {h with loc_label=".globl main\n"^h.loc_label}::t
            | _ -> failwith "empty loc list" in
              locs <- help locs

        method update_loc locs d =
            let identify_des addr1 d1 =
              addr1.loc_addr = d1.func_begin_addr in
            let help d l =
              if identify_des l d then
                {l with loc_label=(d.func_name^" :\n"^l.loc_label)}
              else l in
              List.map (help d) locs

    end


type instr_type = Single | Double | Triple | Four
type instr_dir = Left | Right

class reassemble =

  let data_set = Hashtbl.create 200
  and plt_hash = Hashtbl.create 50
  and pic_hash = Hashtbl.create 3
  and text_set = Hashtbl.create 30 in

  object(self)
    inherit ailVisitor

    val mutable label : (string*int) list = []
    val mutable deslist: string list = []
    val mutable init_array_list: string list = []
    val mutable eh_frame_list: string list = []
    val mutable excpt_tbl_list: string list = []
    val mutable jmpreflist: string list = []
    val mutable sec : section list = []
    val mutable instr_list: instr list = []
    val mutable text_sec: (int*int) = (0,0)  (* begin addr, size*)
    val mutable plt_sec: (int*int) = (0,0)  (* begin addr, size*)
    val mutable text_mem_addrs: string list = []


    (* collect all the symbols from code section or from data sections *)
    val mutable symbol_list : int list = []

    method section_collect =
      let filelines = File.lines_of "sections.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3))
        and secname = List.nth items 0 in
        sec <- {sec_name=secname; sec_begin_addr=addr;
                sec_size=size}::sec
      in
      Enum.iter help filelines;

      text_mem_addrs <- List.map (String.trim) (read_file "text_mem.info")

    method plt_collect =
      let filelines = File.lines_of "plts.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = List.nth items 0 in
        let addr' = "0x"^(String.sub addr 1 (String.length addr - 1))
        and n = List.nth items 1 in
        let items = Str.split (Str.regexp_string "@") n in
        let n' = List.nth items 0 in
        let n'' = String.sub n' 1 (String.length n' - 1) in
        Hashtbl.replace plt_hash (int_of_string addr') n''
      in
      Enum.iter help filelines

    method pic_collect =
      let filelines = File.lines_of "pic_secs.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let name = List.nth items 0
        and addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3)) in
        Hashtbl.replace pic_hash name (addr, size)
      in
      Enum.iter help filelines

    method text_sec_collect =
      let filelines = File.lines_of "text_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3)) in
        text_sec <- (addr, size)
      in
      Enum.iter help filelines

    method plt_sec_collect =
      let filelines = File.lines_of "plt_sec.info"
      and help l =
        let items = Str.split (Str.regexp " +") l in
        let addr = int_of_string ("0x"^(List.nth items 1))
        and size = int_of_string ("0x"^(List.nth items 3)) in
        plt_sec <- (addr, size)
      in
      Enum.iter help filelines


    (*          heriustic analysis
     *        check_sec; check_text; check_plt
     *        Heriustically, we consider any value that fall inside
     *         the range of data/rodata/bss/text/plt sections as suspicious symbols
     *  See this paper's table one for some examples
     *  https://www.utdallas.edu/~zxl111930/file/CCS13.pdf
     *)

    method check_sec addr =
      let rec help l addr =
        match l with
        | h::t -> (
          let b = h.sec_begin_addr in
          let e = b + h.sec_size in
          if (addr>=b && addr < e)  then
            Some (h)
          else  help t addr )
        | [] -> None in
      help sec addr


    method check_text addr =
      let dec_hex n =
  	    (Printf.sprintf "%x:" n) in
      let judge_mem_list addr =
	    let addrs = dec_hex addr in
	    List.mem addrs text_mem_addrs in
      let (b,size) = text_sec in
      let e = b + size in
      if addr = 0xffff then false
      (* this is dangerous, when processing 64-bit shared object, for example,
    ssh, the text section range includes memory address 0xffff. However, I
    found instructions like this in the code:
     cmp $0xffff,%eax
       *)
                              (*  else if (addr>=b && addr < e) && (judge_mem_list addr) then true *)
      else if (addr>=b && addr < e) then true
      else false

    method check_text_abd addr =
      let (b,size) = text_sec in
      let e = b + size in
      if (addr>=b && addr <=e) then true
      else false


    method check_plt addr =
      let (b,size) = plt_sec in
      let e = b + size in
      if (addr>=b && addr <e) then true
      else false

    method parse_const c =
      match c with
      | Point s -> s
      | Normal s -> s

    method build_symbol c =
      let dec_hex (s:int) : string =
        "0x"^(Printf.sprintf "%X" s) in
      match c with
      | Point s -> "S_"^(dec_hex s)
      | Normal s -> "$S_"^(dec_hex s)

    method build_plt_symbol c =
      let dec_hex (s:int) : string =
        (Printf.sprintf "0x%x" s) in
      let c' =
        match c with
        | Point s -> s
        | Normal s -> s in
      try
        let n = Hashtbl.find plt_hash c' in
        match c with
        | Point _ -> n (* is it possible ?*)
        | Normal _ -> ("$"^n)
      with _ -> failwith ("exception in build plt symbol ")

    method has_data l =
      Hashtbl.mem data_set l = true

    method has_text l =
      Hashtbl.mem text_set l = true

    method v_exp2 (e : exp) (i : instr) (f : instr -> bool) : exp =
      let dec_hex (s:int) : string =
        (Printf.sprintf "0x%X" s) in
      match e with
      | Const l ->
         (
           let l' = self#parse_const l in
           match (self#check_sec l') with
           | Some s ->
              (
                if self#has_data l' then
                  (
                    let s_label = (self#build_symbol l) in
                    Label s_label
                  )
                else
                  (
                    Hashtbl.replace data_set l' "";
                    let s_label = (self#build_symbol l) in
                    label <- (s.sec_name, l')::label;
                    Label s_label
                  )
              )
           | None ->
              (
                if (self#check_text l') then
                  (
                    if self#has_text l' then
                      (
					    let rb = f i  in
                        ();

                        let s_label = (self#build_symbol l) in
                        Label s_label
                      )
                    else
                      (
                        Hashtbl.replace text_set l' "";
                        let s_label = (self#build_symbol l) in
                        deslist <- s_label::deslist;
                        Label s_label
                      )
                  )else
                  (
                    if (self#check_plt l') then
                      (
                        let ns = (self#build_plt_symbol l) in
                        Label ns
                      )
                    else e
                  )
              )
         )
      | Symbol s -> begin
          match s with
          | JumpDes (l) ->
             (
               if (self#check_text l) then
                 (
                   if self#has_text l then
                     (
                       let s_label = "S_"^(dec_hex l) in
                       Label s_label
                     )
                   else
                     (
                       Hashtbl.replace text_set l "";
                       let s_label = "S_"^(dec_hex l) in
                       deslist <- s_label::deslist;
                       Label s_label
                     )
                 )
               else e (* is it possible? *)
             )
          | StarDes sd -> Symbol (StarDes (self#v_exp2 sd i f))
          | CallDes f ->
             (
               let nl = String.length f.func_name in
               let fn = String.sub f.func_name 2 (nl-2) in
               let is_dig_loc =
                 try
                   ignore(int_of_string(fn)); true
                 with
                 | _ ->
                 begin
                  symbol_list <- f.func_begin_addr::symbol_list;
		  false
                 end in
               if is_dig_loc = true then
                 (
                   let addr = int_of_string(fn) in
                   if (self#check_text addr) then
                     (
                       if self#has_text addr then
                         (
                           let s_label = "S_"^(dec_hex addr) in
                           Label s_label
                         )
                       else
                         (
                           Hashtbl.replace text_set addr "";
                           let s_label = "S_"^(dec_hex addr) in
                           (* print_string s_label; *)
                           (* print_string "\n"; *)
                           deslist <- s_label::deslist;
                           Label s_label
                         )
                     )
                   else e
                 )
               else e
             )
          | _ -> e
        end
      | Ptr s -> begin
          match s with
          | BinOP_PLUS (r, addr) ->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "S_"^(dec_hex addr) in
                        Ptr (BinOP_PLUS_S (r,s_label))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (BinOP_PLUS_S (r,s_label))
                      )
                  )
               | None -> e
             )
          (* I don't know whether it can go this way, but it doesn't hurt *)
          | BinOP_MINUS (r, addr) ->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "S_"^(dec_hex addr) in
                        Ptr (BinOP_MINUS_S (r,s_label))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (BinOP_MINUS_S (r,s_label))
                      )
                  )
               | None -> e
             )
          | FourOP_PLUS (r1, r2, off, addr)->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "S_"^(dec_hex addr) in
                        Ptr (FourOP_PLUS_S (r1,r2,off,s_label))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (FourOP_PLUS_S (r1,r2,off,s_label))
                      )
                  )
               | None -> e
             )
          (* I don't know whether it can go this way, but it doesn't hurt *)
          | FourOP_MINUS (r1, r2, off, addr)->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "S_"^(dec_hex addr) in
                        Ptr (FourOP_PLUS_S (r1,r2,off,s_label))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (FourOP_PLUS_S (r1,r2,off,s_label))
                      )
                  )
               | None -> e
             )
          | JmpTable_PLUS (addr, r, off) ->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "S_"^(dec_hex addr) in
                        Ptr (JmpTable_PLUS_S (s_label, r, off))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (JmpTable_PLUS_S (s_label, r, off))
                      )
                  )
               | None ->
                  if (self#check_text addr) then
                    (
                      if self#has_text addr then
                        (
                          let s_label = "S_"^(dec_hex addr) in
                          Ptr (JmpTable_PLUS_S (s_label, r, off))
                        )
                      else
                        (
                          Hashtbl.replace text_set addr "";
                          let s_label = "S_"^(dec_hex addr) in
                          deslist <- s_label::deslist;
                          Ptr (JmpTable_PLUS_S (s_label, r, off))
                        )
                    )
                  else
                    e
             )
          | JmpTable_MINUS (addr, r, off) ->
             (
               match (self#check_sec addr) with
               | Some s ->
                  (
                    if self#has_data addr then
                      (
                        let s_label = "-S_"^(dec_hex addr) in
                        Ptr (JmpTable_MINUS_S (s_label, r, off))
                      )
                    else
                      (
                        Hashtbl.replace data_set addr "";
                        let s_label = "-S_"^(dec_hex addr) in
                        label <- (s.sec_name, addr)::label;
                        Ptr (JmpTable_MINUS_S (s_label, r, off))
                      )
                  )
               | None ->
                  if (self#check_text addr) then
                    (
                      if self#has_text addr then
                        (
                          let s_label = "-S_"^(dec_hex addr) in
                          Ptr (JmpTable_MINUS_S (s_label, r, off))
                        )
                      else
                        (
                          Hashtbl.replace text_set addr "";
                          let s_label = "-S_"^(dec_hex addr) in
                          deslist <- s_label::deslist;
                          Ptr (JmpTable_MINUS_S (s_label, r, off))
                        )
                    )
                  else
                    e
             )
          | _ -> e
        end
      | _ -> e

    method vinst2 (f : instr -> bool) (i : instr) : instr =
      match i with
      | SingleInstr (p, l, pre) -> i
      | DoubleInstr (p, e, l, pre) -> DoubleInstr (p, (self#v_exp2 e i f), l, pre)
      | TripleInstr (p, e1, e2, l, pre) -> TripleInstr (p, (self#v_exp2 e1 i f),
                                                        (self#v_exp2 e2 i f), l, pre)
      | FourInstr (p, e1, e2, e3, l, pre) -> FourInstr (p, e1, (self#v_exp2 e2 i f), e3, l, pre)

    (* here is the thing, because our dataflow analysis is based on
     * CFG and CG, however, the contruction of CFG and CG is based on (coarse)
     * symbolization on text segment,
     * so we decided to process twice on the text segment, the
     * first round works on producing a coarse-grained CFG, then CFG/CG can be built
     * based on that, after coarse-grained control flow structure is recovered, we
     * should be able to conduct dataflow analysis to get more precise results
     *
     *    when processing ls binary program, it would take 0.1 more seconds (3.6s -> 3.7s)
     *        2014-09-09
     *)
    method visit_heuristic_analysis (instrs: instr list) =
      let func (i : instr) : bool =
        let l = get_loc i in
        self#check_text l.loc_addr in
      instr_list <- instrs;
      let tl = List.map (self#vinst2 func) instrs in
      let tl1 = List.map (fun l ->
                          try
                            let s' = String.sub l 2 9 in
                            int_of_string s'
                          with
                          | _ ->
                             let s' = String.sub l 3 9 in
                             int_of_string s'
                         )
                         deslist in
      symbol_list <- List.rev_append (List.rev tl1) symbol_list;
      tl

    method visit_static_analysis (bbl : bblock list) (instrs : instr list) =
      (*let's do dataflow analysis on suspicious concerete value*)
      let func (i : instr) : bool =
        print_string ((pp_print_instr i)^"\n");
        let dta = new dataFlow instr_list in
        dta#df_check_const_val i in
      instr_list <- instrs;
      List.map (self#vinst2 func) instrs

    method visit_type_infer_analysis (bbl : bblock list) (instrs : instr list) =
      (*let's do type inference analysis on suspicious concerete value*)
      let func (i : instr) : bool =
		(*
            print_string ("    type inference on : "^(pp_print_instr i));
            TypeInfer.type_infer instrs i;
		 *)
	    true
      in
      instr_list <- instrs;
      List.map (self#vinst2 func) instrs

    (* check whether it is a library or executable *)
	method check_32 =
  	  let filelines : string list = read_file "elf.info" in
      let info = List.nth filelines 0 in
      if String.exists info "32-bit" then
        true
      else
        false

	method check_exe =
  	  let filelines : string list = read_file "elf.info" in
      let info = List.nth filelines 0 in
      if String.exists info "LSB shared object" then
        false
      else
        true

    method share_lib_processing (instrs : instr list) =
      let is_32 = self#check_32 in
      let is_exe = self#check_exe in
      if is_32 = false || is_exe = true then
        instrs
      else
        begin
          let helper = new lib32_helper instrs in
          let labels = helper#traverse in
          (*
              let () = List.iter (fun addr -> print_string ((dec_hex addr)^"\n"))
                                 labels in
           *)
          (* print_int (List.length labels); *)
          let aux addr =
            match (self#check_sec addr) with
            | Some s -> label <- (s.sec_name, addr)::label
            | None -> failwith "unsupport section info in share_lib processing"
          in
          List.iter aux labels;
          helper#get_instrs
        end


    (* redundunt with check_sec *)
    method check_bss addr =
      let rec help l addr =
        match l with
        | h::t -> (
          if h.sec_name = ".bss" then
            let b = h.sec_begin_addr in
            let e = b + h.sec_size in
            if (addr>=b && addr < e)  then
              true
            else  help t addr
          else  help t addr )
        | [] -> false in
      help sec addr

    method update_deslist_with_initarray =
      let _ = Sys.command("python parse_init_array.py") in
      init_array_list <- read_file "init_array_new.info"

    method update_deslist_with_ehframe =
      let _ = Sys.command("python exception_process.py eh_frame") in
      eh_frame_list <- read_file "eh_frame.info"

    method update_deslist_with_excp_tbl =
      let _ = Sys.command("python exception_process.py gcc_exception_table") in
      excpt_tbl_list <- read_file "gcc_exception_table.info"

    method adjust_loclabel instr_list =
      self#update_deslist_with_initarray;
      let t = List.rev_append (List.rev init_array_list) deslist in
      let p = new instrhandler instr_list t in
	  (*
          self#update_deslist_with_ehframe;
          self#update_deslist_with_excp_tbl;
          let p = new instrhandler instr_list deslist in
	   *)

      p#set_instr_list;
      p#process;
      p#get_instr_list

    (* in the data section and rodata section, jmptable could have ref into text section*)
    method adjust_jmpref instr_list =
      let p = new instrhandler instr_list jmpreflist in
      p#set_instr_list;
      p#process;
      p#get_instr_list

    method adjust_funclabel u_funcs instr_list =
      let p = new funchandler instr_list u_funcs in
      p#set_instr_list;
      p#process2;
      p#insert_dummy;
      p#get_instr_list

    method adjust_globallabel g_bss instr_list =
      let g_bss' = List.filter (fun (a,n) ->
                                String.exists n "@") g_bss in
      let labels = List.map (fun (a,n) -> a) g_bss'
      and size = (List.length g_bss') + 1 in
      let gbss_hs = Hashtbl.create size in
      let help (addr, n) =
        let addr' = "S_0x"^addr in
        if String.contains n '@' then (* stderr@@GLIBC_2.0 *)
          let n' = List.nth (Str.split (Str.regexp "@") n) 0 in
          Hashtbl.replace gbss_hs addr' n'
        else Hashtbl.replace gbss_hs addr' n in
      let check l =
        try Some (List.find (fun i-> String.exists l i) labels)
        with Not_found -> None
      and sub l key =
        let key' = ("S_0x"^key) in
        try
          let v = Hashtbl.find gbss_hs key' in
          Str.replace_first (Str.regexp key') v l
        with _ -> failwith "exception in adjust_globallabel" in
      let help' l =
        match check l with
        | Some k -> sub l k
        | None -> l in
      List.iter help g_bss';
      List.map help' instr_list

    method data_dump funcs =
      let dec_hex (s:int) : string =
        "S_0x"^(Printf.sprintf "%X" s) in
      let export_datas = self#export_data_dump in
      let t = List.rev_append (List.rev label) export_datas in
      let p = new datahandler t in
      (* let p = new datahandler (export_datas) in *)
      p#text_sec_collect;
      p#set_datas funcs;
      let templist = p#get_textlabel in
      jmpreflist <- List.map (fun l -> (dec_hex l)) templist;
      p#data_output


    method data_dump_1 =
      let dec_hex (s:int) : string =
        "S_0x"^(Printf.sprintf "%X" s) in
      let export_datas = self#export_data_dump in
      let t = List.rev_append (List.rev label) export_datas in
      let p = new datahandler t in
      p#text_sec_collect;
      p#set_datas_1;
      let templist = p#get_textlabel in
      jmpreflist <- List.map (fun l -> (dec_hex l)) templist;
      List.rev_append (List.rev templist) symbol_list

    method dump_funclist bs fn =
      let dec_hex (s:int) : string =
        "0x0"^(Printf.sprintf "%x" s) in
      let oc = open_out_gen [Open_append; Open_creat] 0o666 fn in
      List.iter (fun l -> Printf.fprintf oc "%s\n" (dec_hex l)) bs;
      close_out oc

    (* unify symbol list and pattern match list, to get the unified function
    list *)
    method unify_funclist sl1 sl2 =
      self#dump_funclist sl1 "union_1.txt";
      self#dump_funclist sl2 "union_2.txt";
      let sl1 = unify_int_list sl1 in
      let sl2 = unify_int_list sl2 in
      let sl1 = List.sort Int.compare sl1 in
      let sl2 = List.sort Int.compare sl2 in
      let rec intersect l1 l2 =
        match l1 with [] -> []
                    | h1::t1 -> (
                      match l2 with [] -> []
                                  | h2::t2 when h1 < h2 -> intersect t1 l2
                                  | h2::t2 when h1 > h2 -> intersect l1 t2
                                  | h2::t2 -> (
                                    match intersect t1 t2 with [] -> [h1]
                                                             | h3::t3 as l when h3 = h1 -> l
                                                             | h3::t3 as l -> h1::l
                                  )
                    ) in
      print_int (List.length sl1);
      print_string "\n";
      print_int (List.length sl2);
      print_string "\n";
      intersect sl1 sl2


    method init_array_dump =
	  if List.length init_array_list <> 0 then
	    begin
          let oc = open_out_gen [Open_append; Open_creat] 0o666 "final_data.s" in
          Printf.fprintf oc "\n.section        .ctors,\"aw\",@progbits\n.align 4\n";
          List.iter (fun l -> Printf.fprintf oc ".long %s\n" l) init_array_list;
          close_out oc
	    end
	  else ()

    method export_data_dump =
      Sys.command("python export_data.py");
      let addrs = List.map (String.trim) (read_file "export_datas.info") in
      let aux l =
        match (self#check_sec l) with
        | Some s -> (s.sec_name, l)
        | None   -> failwith "unsupport export data"
      in
      List.map (fun ls -> aux (int_of_string ls) ) addrs

    method ehframe_dump =
      Sys.command("cat eh_frame.data >> final.s");

    method excpt_tbl_dump =
      Sys.command("cat gcc_exception_table.data >> final.s");

    method reassemble_dump u_funcs =
      self#data_dump u_funcs;
      self#init_array_dump

    method reassemble_dump_1 =
      self#data_dump_1

    method add_func_label ufuncs instrs =
      let rec help acc fl il =
        match (fl,il) with
        | ([], il') -> List.rev_append il' acc
        | (hf::tf, hi::ti) ->
           begin
             let iloc = get_loc hi in
             if hf.func_begin_addr = iloc.loc_addr then
               let iloc' = {iloc with
                             loc_label = hf.func_name^":\n"^iloc.loc_label} in
               let hi' = set_loc hi iloc' in
               help acc tf (hi'::ti)
             else if hf.func_begin_addr < iloc.loc_addr then
               help acc tf il
             else
               help (hi::acc) fl ti
           end
        | (hf::tf, []) ->
           begin
             (* failwith ("failed in adding function "^(hf.func_name)^" in add_func_label"); *)
             acc
           end in
      List.rev (help [] ufuncs instrs)

    method add_bblock_label bbl instrs =
      let rec help acc bl il =
        match (bl,il) with
        | ([], il') -> List.rev_append il' acc
        | (ib::tb, hi::ti) ->
           begin
             let iloc = get_loc hi
             and bloc = ib.bblock_begin_loc in
             if bloc.loc_addr = iloc.loc_addr then
               let iloc' = {iloc with
                             loc_label = ib.bblock_name^":\n"^iloc.loc_label} in
               let hi' = set_loc hi iloc' in
               help (hi'::acc) tb ti
             else
               help (hi::acc) bl ti
           end
        | _ -> failwith "failed in add_bblock_label" in
      let bbl' = List.sort (
                     fun b1 b2 ->
                     b1.bblock_begin_loc.loc_addr - b2.bblock_begin_loc.loc_addr
                   ) bbl in
      List.rev (help [] bbl' instrs)


    method pp_print_1 l =
      let rec help l =
        match l with
        | h::t -> (printf "item: %s ") h; help t
        | [] -> print_string "end\n" in
      help l


    method unify_loc1 instrs =
      let p = object
          val mutable last_label = ""
          val mutable remove_l = []
          method update_instrs instrs =
            remove_l <- List.rev remove_l;
            let rec help il rl acc =
              match (il, rl) with
              | (ih::it, rh::rt) ->
                 begin
                   let lo = get_loc ih in
                   if lo.loc_addr = rh then
                     begin
                       let ih' = set_loc ih {loc_label = "";
                                             loc_addr = lo.loc_addr;loc_visible=true;} in
                       help it rt (ih'::acc)
                     end
                   else
                     help it rl (ih::acc)
                 end
              | (il', []) -> List.rev_append acc il'
              | ([], rl') -> failwith "can't find coresponding address in unify_loc" in
            help instrs remove_l []
          method set_list instrs =
            let dec_hex (s:int) : string =
              "0x"^(Printf.sprintf "%X" s) in
            let help i =
              let lo = get_loc i in
              if lo.loc_label <> "" && lo.loc_label = last_label then (* remove it *)
                begin
                  (* bug :  new inserted instruction would have same location
                              with the above instruction, which means it will remove
                            the above instruction's label *)
                  remove_l <- (lo.loc_addr)::remove_l;
                  print_string last_label;
                  print_string "\n";
                  print_string (dec_hex lo.loc_addr);
                  print_string "\n";
                end
              else
                last_label <- lo.loc_label in
            List.iter help instrs
        end in
      p#set_list instrs;
      p#update_instrs instrs


    method unify_loc instrs =
      let p = object
          val mutable last_label = ""
          method unify_instrs instrs =
            let rec help acc il =
              match il with
              | [] -> acc
              | ih::it ->
                 begin
                   let lo = get_loc ih in
                   if lo.loc_label <> ""
                      && lo.loc_label = last_label then (* remove it *)
                     begin
                       let ih' = set_loc ih {loc_label = "";
                                             loc_addr = lo.loc_addr;loc_visible=true;} in
                       help (ih'::acc) it
                     end
                   else
                     begin
                       last_label <- lo.loc_label;
                       help (ih::acc) it
                     end
                 end in
            List.rev (help [] instrs)
        end in
      p#unify_instrs instrs

    initializer
      self#section_collect;
      self#plt_collect;
      self#plt_sec_collect;
      self#text_sec_collect

  end
