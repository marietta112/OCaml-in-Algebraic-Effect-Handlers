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
  (* If the previous number was even (resp. odd) and the current number is even (resp. odd) then
     we print an error statement on the screen. Otherwise, nothing happens and in all cases 
     the continuation is resumed by giving it the unit value. *)
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_temporal_mon () = temp_monitor ()