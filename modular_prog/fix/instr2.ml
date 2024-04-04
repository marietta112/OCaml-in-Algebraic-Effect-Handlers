open Utils
open Effect
open Effect.Deep
open Modular_program2
open Printf

let report_p n = Utils.E.trace_lst := n::!(Utils.E.trace_lst)
(* *Generates trace by storing integer in a list *)

let instrumentation x = 
    match_with (Modular_program2.comp) x
    (* The computation passed must be of type unit -> a, for some type a. *)
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Utils.E.Put s -> 
          Some (fun (k : (c,_) continuation) -> report_p(s); continue k ())
          (* The continuation type is a pair (x, y) - x is the type of the value passed to 
             the continuation for coputation to resume and y is the type of the value returned by
             the continuation. *)
        | _ -> None 
        (* Forwards unhandled effects to the outer handler *)
      );
      exnc = raise; 
      (* Optional *)
      retc = fun x -> x 
      (* Required *)
    }
  (* We return a function instead of the match_with clause since this handler is passed to another 
     handler as an argument. *)  

let run_inst2 () =  instrumentation ()