open Effect
open Effect.Deep
open Printf
open Utils

(* [monitor1] is the stateless monitor, the first most basic monitor described using 
   an effect handler. With each [Put], it checks if the argument passed is -1. If it is, an 
    error message is printed on the console. *)

let monitor1 x = match_with (Effectful_program.main) x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    (* Store the passed value [s] in [loc] and check if it is -1. Assuming that [loc] is found in [Utils] *)
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> (!loc).value <- s; 
      if s = (-1) then (printf "Put with value -1 encountered.\n"; discontinue k (Utils.E.Invalid_value s))
      else continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> 
      if s = (-1) then (printf "Put with value -1 encountered.\n"; discontinue k (Utils.E.Invalid_value s))
      else continue k ())
    (* If a [Get] is found, ignore and continue. *)    
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> continue k ((!x).value))
    (* If a [Alias] is found, ignore and continue. *)    
    | Utils.E.Alias (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    (* If a [Check] is found, ignore and continue. *)    
    | Utils.E.Check (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_mon1 () = monitor1 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [sum_monitor] is a stateful monitor that keeps a running total of the integers stored across all
   the memory locations individually. 
   Should the sum exceed 500, an error message is printed on the console. *)
  let sum_monitor x = match_with (Effectful_program.main) x
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Init (loc, s) -> 
        Some (fun (k: (b,_) continuation) -> (!loc).value <- s; continue k ())
      | Utils.E.Put (loc,s) -> 
        Some (fun (k: (b,_) continuation) ->
        if !(Utils.E.sum) < 500 then (Utils.E.sum := !(Utils.E.sum) + s; continue k ())
        else printf "Total sum of puts exceeded 500.\n" ; discontinue k (Utils.E.Exceeded_value 500))
      | Utils.E.Get x -> 
        Some (fun (k: (b,_) continuation) -> continue k ((!x).value))
      (* If a [Alias] is found, ignore and continue. *)    
      | Utils.E.Alias (x,y) -> 
        Some (fun (k: (b,_) continuation) -> continue k ())
      (* If a [Check] is found, ignore and continue. *)    
      | Utils.E.Check (x,y) -> 
        Some (fun (k: (b,_) continuation) -> continue k ())
      | _ -> None
    );
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  }
  
  let run_sum_mon () = sum_monitor ()
   
(* ---------------------------------------------------------------------------------------------------------------------------- *)  
(* [sum_monitor2] is a stateful monitor that keeps a running total of the integers stored across one 
   memory location. 
   Should the sum exceed 500 for one particular memory location, 
   an error message is printed on the console and the program is terminated by an exception. *)
let sum_monitor2 x = match_with (Effectful_program.main) x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> (!loc).value <- s; continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) ->
      if (!loc).total < 500 then ((!loc).total <- (!loc).total + s; continue k ())
      else printf "Total sum of puts in one memory location exceeded 500.\n" ; discontinue k (Utils.E.Exceeded_value 500))
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> continue k ((!x).value))
    (* If a [Alias] is found, ignore and continue. *)    
    | Utils.E.Alias (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    (* If a [Check] is found, ignore and continue. *)    
    | Utils.E.Check (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_sum_mon2 () = sum_monitor2 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [temp_monitor] is a stateful monitor that checks whether puts are alternating between odd and
   even integers. If the property is violated, an error message is printed on the console. *)
let temp_monitor x = match_with (Effectful_program.main) x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> (!loc).value <- s; continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) ->
          if !Utils.E.flag == 2 then (if s mod 2 == 0 then (Utils.E.flag := 0; continue k ()) else (Utils.E.flag := 1; continue k ()))
          else if (s mod 2 == 0) then 
            (if !Utils.E.flag == 0 then (printf "Two consecutive evens found.\n"; discontinue k Utils.E.Not_alternating)
                                      else Utils.E.flag := 0; continue k ())
          else (if !Utils.E.flag == 1 then (printf "Two consecutive odds found.\n"; discontinue k Utils.E.Not_alternating) 
                                          else Utils.E.flag := 1; continue k ()) 
        )
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> continue k ((!x).value))
    (* If a [Alias] is found, ignore and continue. *)    
    | Utils.E.Alias (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    (* If a [Check] is found, ignore and continue. *)    
    | Utils.E.Check (x,y) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())    
    | _ -> None
  );
  (* If [flag] has value 2, then, the [Put] we have encountered is the first one, therefore, we 
     update [flag] to 0 if the integer passed to [Put] is even and 1 if it is odd. 
     
     If [flag] has value 0 or 1, then, the current [Put] is not the first one. If the 
     integer passed to [Put] is even and [flag] is 0, then the previous integer was also even 
     and and error is reported to the screen. However, if [flag] is set to 1, then this is allowed,
     we update [flag] to 0 for the next iteration. Similarly for the case when the integer passed
     to [Put] is odd. *)
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_temporal_mon () = temp_monitor ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [monitor2_l1] keeps track of the integers passed to [put] by storing the most recent one in [prev]. *)
(* let mon2_l1 x = fun () -> 
  match_with (Effectful_program.main) x
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Put (loc,s) -> 
        Some (fun (k: (b,_) continuation) -> Utils.E.prev := s; continue k ()
          )
      | _ -> None
    );
    
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  } <- was commented since it can be merged into one handler with [Get] & since we can have multiple memory locations. *)

  let mon2_l2 f = match_with (Effectful_program.main) ()
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Init (loc, s) -> 
        Some (fun (k: (b,_) continuation) -> (!loc).value <- s; continue k ())
      | Utils.E.Put (loc,s) -> 
        Some (fun (k: (b,_) continuation) -> (!loc).prev <- s; continue k ())
      | Utils.E.Get y -> 
        Some (fun (k: (b,_) continuation) -> 
          if (!y).value != (!y).prev 
          then (printf "The previous [put] stored value %d but the current [get] retrieved value %d.\n" (!y.prev) (!y.value); discontinue k Utils.E.Inconsistent)
          else (printf "[get] retrieved the same value as the previous [put].\n"; continue k ((!y).value)) )
      (* If a [Alias] is found, ignore and continue. *)    
      | Utils.E.Alias (x,y) -> 
        Some (fun (k: (b,_) continuation) -> continue k ())
      (* If a [Check] is found, ignore and continue. *)    
      | Utils.E.Check (x,y) -> 
        Some (fun (k: (b,_) continuation) -> continue k ())
      | _ -> None
    );
    
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  }

let run_mon2 () =  mon2_l2 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [mon3_l1] is a monitor that keeps track of the last integer passed to [Put] and
   keeps a running total of the sum of these integers. An error is reported if the sum
   exceeds 500. It also check that whenever we try to retreive an integer, its value should be
   the same value passed to the most recent [put]. This monitor combines [sum_monitor], [mon2_l1] 
   and [mon2_l1]. *)

let mon3_l1 x = 
fun () ->  
  match_with (Effectful_program.main) x
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Put (loc,s) -> 
        Some (fun (k: (b,_) continuation) -> Utils.E.prev := s;
        if !(Utils.E.sum) < 500 then (Utils.E.sum := !(Utils.E.sum) + s;continue k ())
        else printf "Total sum of puts exceeded 500.\n"; discontinue k (Utils.E.Exceeded_value 500))
      (* | Utils.E.Get (y, ()) -> 
        Some (fun (k: (b,_) continuation) -> 
          if !y != !Utils.E.prev 
          then printf "The previous [put] stored value %d but the current [get] retrieved value %d.\n" (!Utils.E.prev) (!y)
          else printf "[get] retrieved the same value as the previous [put].\n"; continue k (y.value) ) *)
      | _ -> None
    );
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  }
(* mon3_l1 () *)
let run_mon3 () = mon2_l2(mon3_l1 ()) 

(* Alternatively, this can all be done in one handler by removing 'fun () ->' in line 124,
   uncomment the section on [Get] in the [effc] field and simply define [run_mon3] as mon3_l1 (). *)
(* --------------------------------------------------------------------------------------------------------------- *)
let alias_monitor () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    (* Simply store the passed value [s] in [loc]. Assuming that [loc] is found in [Utils] *)
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> (!loc).value <- s; continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> continue k ())
    (* If a [Get] is found, ignore and continue. *)    
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> continue k ((!x).value))
    | Utils.E.Alias (x, y) -> 
      Some (fun (k: (b,_) continuation) -> print_int (!Utils.E.mem1).value; print_endline ""; print_int (!Utils.E.mem2).value; print_endline "";
                                           if (!x).value != (!y).value then discontinue k Utils.E.Alias_Error
                                            else continue k () )
    | Utils.E.Check (x, y) -> 
      Some (fun (k: (b,_) continuation) -> if !x.value != !y.value then discontinue k Utils.E.Inconsistent
                                            else continue k () )
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_alias_mon () = alias_monitor ()