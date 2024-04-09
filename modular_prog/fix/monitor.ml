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
    | Utils.E.Put s -> 
      Some (fun (k: (b,_) continuation) -> 
      if s = (-1) then printf "Put with value -1 encountered.\n"; continue k ())
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_mon1 () = monitor1 ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [sum_monitor] is a stateful monitor that keeps a running total of the integers stored in memory. 
   Should the sum exceed 500, an error message is printed on the console. *)
let sum_monitor x = match_with (Effectful_program.main) x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Put s -> 
      Some (fun (k: (b,_) continuation) ->
      if !(Utils.E.sum) < 500 then Utils.E.sum := !(Utils.E.sum) + s
      else printf "Total sum of puts exceeded 500.\n"; continue k ())
    | _ -> None
  );
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_sum_mon () = sum_monitor ()

(* ---------------------------------------------------------------------------------------------------------------------------- *)

(* [temp_monitor] is a stateful monitor that checks whether puts are alternating between odd and
   even integers. If the property is violated, an error message is printed on the console. *)
let temp_monitor x = match_with (Effectful_program.main) x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Put s -> 
      Some (fun (k: (b,_) continuation) ->
          if !Utils.E.flag == 2 then (if s mod 2 == 0 then Utils.E.flag := 0 else Utils.E.flag := 1)
          else if (s mod 2 == 0) then 
            (if !Utils.E.flag == 0 then printf "Two consecutive evens found.\n"
                                      else Utils.E.flag := 0)
          else (if !Utils.E.flag == 1 then printf "Two consecutive odds found.\n" 
                                          else Utils.E.flag := 1) ; continue k ()
        )
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
let mon2_l1 x = fun () -> 
  match_with (Effectful_program.main) x
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Put s -> 
        Some (fun (k: (b,_) continuation) -> Utils.E.prev := s; continue k ()
          )
      | _ -> None
    );
    
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  }
(* Here we must return a function since [mon2_l1] will be passed to [mon_l2] instead of the computation. *)

  let mon2_l2 f = match_with f ()
  {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Utils.E.Get (y, ()) -> 
        Some (fun (k: (b,_) continuation) -> 
          if !y != !Utils.E.prev 
          then printf "The previous [put] stored value %d but the current [get] retrieved value %d.\n" (!Utils.E.prev) (!y)
          else printf "[get] retrieved the same value as the previous [put].\n"; continue k (!y) )
      | _ -> None
    );
    
    exnc = raise; (* Optional *)
    retc = fun x-> x (* Required *)
  }

let run_mon2 () =  mon2_l2(mon2_l1 ())