open Effect
open Effect.Deep
open Printf

type t = unit 
type c = unit
type _ Effect.t += Report : int -> unit Effect.t 

(* we pass the instrumentation handler as an argument *)
let monitor inst x = match_with inst x
{
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Report n -> Some (fun (k: (b,_) continuation) -> 
        if n = (-1) then printf "Put with value -1 encountered.\n" else printf "Put found with allowed value.\n"; continue k ())
      | _ -> None
    );
    exnc = raise;
    retc = fun x-> x
}

let run_mon f () = monitor f ()
