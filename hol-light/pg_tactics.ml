(* ========================================================================= *)
(* HOL Light subgoal package amended for Proof General and Prooftree.        *)
(*                                                                           *)
(* Mark Adams, David Aspinall.						     *)
(* LFCS, School of Informatics, University of Edinburgh			     *)
(*                                                                           *)
(* (c) Copyright University of Edinburgh and authors, 2012.                  *)
(*									     *)
(* This file contains some functions that are modified from the		     *)
(* original HOL Light code, and is therefore subject to the HOL Light	     *)
(* copyright, see the file LICENSE-HOL-LIGHT in this directory.		     *)
(*									     *)
(* ========================================================================= *)
(*									     *)
(* This file overwrites HOL Light's subgoal package commands with variants   *)
(* that output additional annotations specifically for Prooftree.  These get *)
(* intercepted by Proof General, which removes them from the output          *)
(* displayed to the Proof General user.					     *)

(* TODO: 
   1. add urgent message annotations for errors and strings output during
      long-running tactics
   2. fix on/off: don't turn off prompt annotation, support Prooftree on/off.
*)

(* ------------------------------------------------------------------------- *)
(* Goal counter for providing goal ids.                                      *)
(* ------------------------------------------------------------------------- *)

type goal_id = int;;

let string_of_goal_id id = string_of_int id;;

let the_goal_counter = ref (0 : goal_id);;

let inc_goal_counter () =
  (the_goal_counter := !the_goal_counter + 1);;

let reset_goal_counter () =
  (the_goal_counter := 0;
   !the_goal_counter);;

(* ------------------------------------------------------------------------- *)
(* An xgoal extends a goal with an identity.                                 *)
(* ------------------------------------------------------------------------- *)

type xgoal = goal * goal_id;;

let dest_xgoal (gx : xgoal) = gx;;

(* ------------------------------------------------------------------------- *)
(* The xgoalstate is like goalstate but for xgoals instead of goals.         *)
(* ------------------------------------------------------------------------- *)

type xgoalstate = (term list * instantiation) * xgoal list * justification;;

(* ------------------------------------------------------------------------- *)
(* A goalstack but for xgoals.  Overwrites original HL datatype.             *)
(* ------------------------------------------------------------------------- *)

type goalstack = xgoalstate list;;

(* ------------------------------------------------------------------------- *)
(* A refinement but for xgoals.                                              *)
(* ------------------------------------------------------------------------- *)

type xrefinement = xgoalstate -> xgoalstate;;

(* ------------------------------------------------------------------------- *)
(* Instantiation of xgoals.                                                  *)
(* ------------------------------------------------------------------------- *)

let (inst_xgoal:instantiation->xgoal->xgoal) =
  fun p ((thms,w),id) ->
    (map (I F_F INSTANTIATE_ALL p) thms,instantiate p w),id;;

(* ------------------------------------------------------------------------- *)
(* A printer for xgoals and xgoalstacks.                                     *)
(* ------------------------------------------------------------------------- *)

let the_new_goal_ids = ref ([] : goal_id list);;

let the_tactic_flag = ref false;;

let print_string_seplist sep xs =
  if (xs = [])
    then ()
    else (print_string (hd xs);
          do_list (fun x -> print_string sep; print_string x) (tl xs));;

let print_xgoal ((g,id) : xgoal) : unit =
  ((if (!pg_mode)
      then (print_string ("[Goal ID " ^ string_of_goal_id id ^ "]");
            print_newline ()));
   print_goal g);;

let (print_xgoalstack:goalstack->unit) =
  let print_xgoalstate k gs =
    let (_,gl,_) = gs in
    let n = length gl in
    let s = if n = 0 then "No subgoals" else
              (string_of_int k)^" subgoal"^(if k > 1 then "s" else "")
           ^" ("^(string_of_int n)^" total)" in
    print_string s; print_newline();
    if gl = [] then () else
    (do_list (print_xgoal o C el gl) (rev(1--(k-1)));
     (if (!pg_mode) then print_string "[*]");
     print_xgoal (el 0 gl)) in
  fun l ->
   ((if (!pg_mode) & (!the_tactic_flag)
       then let xs = map string_of_int (!the_new_goal_ids) in
            (the_tactic_flag := false;
             print_string  "[New Goal IDs: ";
             print_string_seplist " " xs;
             print_string "]";
             print_newline ()));
    (if l = [] then print_string "Empty goalstack"
     else if tl l = [] then
       let (_,gl,_ as gs) = hd l in
       print_xgoalstate 1 gs
     else
       let (_,gl,_ as gs) = hd l
       and (_,gl0,_) = hd(tl l) in
       let p = length gl - length gl0 in
       let p' = if p < 1 then 1 else p + 1 in
       print_xgoalstate p' gs);
    (if (!pg_mode) then
     let (vs,theta) =
        if (l = []) then ([],[])
                    else let ((vs,(_,theta,_)),_,_) = hd l in
                         (vs,theta) in
     let foo v =
        let (x,_) = dest_var v in
        x ^ if (can (rev_assoc v) theta) then " using" else " open" in
     let xs = map foo vs in
     (print_newline();
      print_string "(dependent evars: ";
      print_string_seplist ", " xs;
      print_string ")";
      print_newline ())));;

(* ------------------------------------------------------------------------- *)
(* Goal labelling, for fresh xgoals.                                         *)
(* ------------------------------------------------------------------------- *)

let label_goals (gs : goal list) : xgoal list =
  let gxs = map (fun g -> inc_goal_counter (); (g, !the_goal_counter))
                gs in
  (the_new_goal_ids := map snd gxs;
   gxs);;

(* ------------------------------------------------------------------------- *)
(* Version of 'by' for xrefinements.                                         *)
(* ------------------------------------------------------------------------- *)

let (xby:tactic->xrefinement) =
  fun tac ((mvs,inst),glsx,just) ->
    let gx = hd glsx
    and oglsx = tl glsx in
    let (g,id) = dest_xgoal gx in
    let ((newmvs,newinst),subgls,subjust) = tac g in
    let subglsx = label_goals subgls in
    let n = length subglsx in
    let mvs' = union newmvs mvs
    and inst' = compose_insts inst newinst
    and glsx' = subglsx @ map (inst_xgoal newinst) oglsx in
    let just' i ths =
      let i' = compose_insts inst' i in
      let cths,oths = chop_list n ths in
      let sths = (subjust i cths) :: oths in
      just i' sths in
    (mvs',inst'),glsx',just';;

(* ------------------------------------------------------------------------- *)
(* Rotate but for xgoalstate.  Only change is different ML datatype.         *)
(* ------------------------------------------------------------------------- *)

let (xrotate:int->xrefinement) =
  let rotate_p (meta,sgs,just) =
    let sgs' = (tl sgs)@[hd sgs] in
    let just' i ths =
      let ths' = (last ths)::(butlast ths) in
      just i ths' in
    (meta,sgs',just')
  and rotate_n (meta,sgs,just) =
    let sgs' = (last sgs)::(butlast sgs) in
    let just' i ths =
      let ths' = (tl ths)@[hd ths] in
      just i ths' in
    (meta,sgs',just') in
  fun n -> if n > 0 then funpow n rotate_p
           else funpow (-n) rotate_n;;

(* ------------------------------------------------------------------------- *)
(* Perform refinement proof, tactic proof etc.                               *)
(* ------------------------------------------------------------------------- *)

let (mk_xgoalstate:goal->xgoalstate) =
  fun (asl,w) ->
    if type_of w = bool_ty then
      null_meta,[((asl,w), reset_goal_counter ())],
      (fun inst [th] -> INSTANTIATE_ALL inst th)
    else failwith "mk_goalstate: Non-boolean goal";;

(* ------------------------------------------------------------------------- *)
(* The Prooftree global state is an ever increasing counter.                 *)
(* ------------------------------------------------------------------------- *)

let the_pt_global_state = ref 1;;

let inc_pt_global_state () =
  (the_pt_global_state := !the_pt_global_state + 1);;

let pt_global_state () = !the_pt_global_state;;

(* ------------------------------------------------------------------------- *)
(* The Prooftree proof state is the length of the goal stack.                *)
(* ------------------------------------------------------------------------- *)

let the_current_xgoalstack = ref ([] : goalstack);;

let pt_proof_state () = length !the_current_xgoalstack;;

let pt_back_to_proof_state n : goalstack =
  let pst = pt_proof_state () in
  if (0 <= n) & (n <= pst)
    then (the_current_xgoalstack :=
               snd (chop_list (pst-n) !the_current_xgoalstack);
          !the_current_xgoalstack)
    else failwith "Not a valid Prooftree state number";;

(* ------------------------------------------------------------------------- *)
(* Subgoal package but for xgoals.  These overwrite the existing commands.   *)
(* ------------------------------------------------------------------------- *)

let (xrefine:xrefinement->goalstack) =
  fun r ->
    let l = !the_current_xgoalstack in
    let h = hd l in
    let res = r h :: l in
    the_current_xgoalstack := res;
    !the_current_xgoalstack;;

let flush_goalstack() =
  let l = !the_current_xgoalstack in
  the_current_xgoalstack := [hd l];;

let e tac =
  let result = xrefine(xby(VALID tac)) in
  (inc_pt_global_state ();
   the_tactic_flag := true;
   result);;

let r n =
  (inc_pt_global_state ();
   xrefine(xrotate n));;

let set_goal(asl,w) =
  let aths = map ASSUME asl in
  (inc_pt_global_state ();
   the_current_xgoalstack :=
     [mk_xgoalstate(map (fun th -> "",th) aths,w)];
   !the_current_xgoalstack);;

let g t =
  let fvs = sort (<) (map (fst o dest_var) (frees t)) in
  (if fvs <> [] then
     let errmsg = end_itlist (fun s t -> s^", "^t) fvs in
     warn true ("Free variables in goal: "^errmsg)
   else ());
  set_goal([],t);;

let b() =
  let l = !the_current_xgoalstack in
  if length l = 1 then failwith "Can't back up any more" else
  (inc_pt_global_state ();
   the_current_xgoalstack := tl l;
   !the_current_xgoalstack);;

let p() = !the_current_xgoalstack;;

let top_realgoal() =
  let (_,(((asl,w),id)::_),_)::_ = !the_current_xgoalstack in
  asl,w;;

let top_goal() =
  let asl,w = top_realgoal() in
  map (concl o snd) asl,w;;

let top_thm() =
  let (_,[],f)::_ = !the_current_xgoalstack in
  f null_inst [];;

(* ------------------------------------------------------------------------- *)
(* Configure the annotated prompt.				             *)
(* ------------------------------------------------------------------------- *)

pg_prompt_info := 
   fun () -> 
   let pst = pt_proof_state () and gst = pt_global_state () in
   string_of_int gst ^ "|" ^ string_of_int pst;;

(* ------------------------------------------------------------------------- *)
(* Printing the goal of a given Prooftree goal id.                           *)
(* ------------------------------------------------------------------------- *)

let print_xgoal_of_id (id:goal_id) : unit =
  let gsts = !the_current_xgoalstack in
  let find_goal (_,xgs,_) = find (fun (g,id0) -> id0 = id) xgs in
  let xg = tryfind find_goal gsts in
  print_xgoal xg;;

(* ------------------------------------------------------------------------- *)
(* Install the new goal-related printers.                                    *)
(* ------------------------------------------------------------------------- *)

#install_printer print_xgoal;;
#install_printer print_xgoalstack;;
