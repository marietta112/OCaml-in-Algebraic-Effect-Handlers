open Effect
open Effect.Deep
open Printf
open Modular_program
open Effects

let report_p n = printf "%d\n"(n);perform(Effects.E.Report n)
(* *Generates trace by printing the passed value, then performs the effect [Report] *)

let instrumentation () = fun () -> match_with (Modular_program.comp) ()
{ effc = (fun (type c) (eff: c Effect.t) ->
    match eff with
    | Effects.E.Put s -> Some (fun (k : (c,_) continuation) ->
            report_p(s); continue k ())
    | _ -> None
  );
  exnc = raise;
  retc = fun x-> x
}

let run_inst () =  instrumentation ()