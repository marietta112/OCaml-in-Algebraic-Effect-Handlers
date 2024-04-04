open Effect
open Effect.Deep
open Printf
open Instr
open Effects

let monitor inst = match_with inst ()
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Effects.E.Report n -> 
      Some (fun (k: (b,_) continuation) -> 
      if n = (-1) then printf "Put with value -1 encountered.\n" 
                  else printf "Put found with allowed value.\n"; continue k ())
    | _ -> None
  );
  (* If an effect is performed by [inst], then we have the type of the raised effect and its
     continuation k. If the types match, then we can evaluate the handler's code. Otherwise, 
     an exception is raised. *)
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_mon f = monitor f