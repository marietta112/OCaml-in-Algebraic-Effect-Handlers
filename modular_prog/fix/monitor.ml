open Effect
open Effect.Deep
open Printf
open Utils

(* [monitor1] is the stateless monitor, the first most basic monitor described using 
   an effect handler. With each [Put], it checks if the argument passed is -1. If it is, an 
    error message is printed on the console. *)

let monitor1 () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    (* Store the passed value [s] in [loc] and check if it is -1. Assuming that [loc] is found in [Utils] *)
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s);
      if s = (-1) then (printf "Put with value -1 encountered.\n"; discontinue k (Utils.E.Invalid_value s))
      else continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> 
      if s = (-1) then (printf "Put with value -1 encountered.\n"; discontinue k (Utils.E.Invalid_value s))
      else continue k ())
    (* If a [Get] is found, ignore and continue. *)    
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> let x = Utils.E.get_entry Utils.E.names_hash x in continue k (!x))
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
let sum_monitor () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s); continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> 
      if !(Utils.E.sum) < 500 then (Utils.E.sum := !(Utils.E.sum) + s; continue k ())
      else printf "Total sum of puts exceeded 500.\n" ; discontinue k (Utils.E.Exceeded_value 500))
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> let x = Utils.E.get_entry Utils.E.names_hash x in continue k (!x))
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
let sum_monitor2 () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s); continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> 
      let x = Utils.E.get_entry Utils.E.sums_hash loc in let y = (!x) + s in Utils.E.update_entry Utils.E.sums_hash loc (ref y);
      if (!x + s) < 500 then continue k ()
      else printf "Total sum of puts in one memory location exceeded 500.\n" ; discontinue k (Utils.E.Exceeded_value 500))
    (* If a [Get] is found, ignore and continue. *)    
    | Utils.E.Get x -> 
      Some (fun (k: (b,_) continuation) -> let x = Utils.E.get_entry Utils.E.names_hash x in continue k (!x))
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_sum_mon2 () = sum_monitor2 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [temp_monitor] is a stateful monitor that checks whether puts are alternating between odd and
   even integers. If the property is violated, an error message is printed on the console. *)
let temp_monitor () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s); continue k ())
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
      Some (fun (k: (b,_) continuation) -> let x = Utils.E.get_entry Utils.E.names_hash x in continue k (!x))
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
let mon2_l2 () = match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s); continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.prev_hash loc (ref s); continue k ())
    | Utils.E.Get y -> 
      Some (fun (k: (b,_) continuation) -> 
        let current = Utils.E.get_entry Utils.E.names_hash y in let prev = Utils.E.get_entry Utils.E.prev_hash y in
        if (!current) != (!prev) 
        then (printf "The previous [put] stored value %d but the current [get] retrieved value %d.\n" (!prev) (!current); discontinue k Utils.E.Inconsistent)
        else (printf "[get] retrieved the same value as the previous [put].\n"; continue k (!current)))
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_mon2 () =  mon2_l2 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

let two_props () = 
match_with (Effectful_program.main) ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Init (loc, s) -> 
      Some (fun (k: (b,_) continuation) -> Utils.E.update_entry Utils.E.names_hash loc (ref s); continue k ())
    | Utils.E.Put (loc,s) -> 
      Some (fun (k: (b,_) continuation) -> 
      if !(Utils.E.sum) < 500 then (Utils.E.sum := !(Utils.E.sum) + s; continue k ())
      else printf "Total sum of puts exceeded 500.\n" ; discontinue k (Utils.E.Exceeded_value 500))
    | Utils.E.Get y -> 
      Some (fun (k: (b,_) continuation) -> 
        let current = Utils.E.get_entry Utils.E.names_hash y in let prev = Utils.E.get_entry Utils.E.prev_hash y in
        if (!current) != (!prev) 
        then (printf "The previous [put] stored value %d but the current [get] retrieved value %d.\n" (!prev) (!current); discontinue k Utils.E.Inconsistent)
        else (printf "[get] retrieved the same value as the previous [put].\n"; continue k (!current)))
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}
let run_mon_props () = two_props ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

let run_mon3 () = sum_monitor (); mon2_l2 ()