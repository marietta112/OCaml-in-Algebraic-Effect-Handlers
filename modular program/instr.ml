open Effects
open Effect
open Effect.Deep
open Modular_program
open Printf

let report_p n = printf "%d\n"(n);perform(Effects.E.Report n)
(* *Generates trace by printing the passed value, then performs the effect [Report] *)

let instrumentation x = 
  fun () -> 
    match_with (Modular_program.comp) x
    (* The computation passed must be of type unit -> a, for some type a. *)
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Effects.E.Put s -> 
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

let run_inst () =  instrumentation ()