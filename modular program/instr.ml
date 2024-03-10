open Effect
open Effect.Deep
open Printf
open Modular_program
open Effects

(* type t = unit
type s = unit *)
(* type r = unit *)

(* type _ Effect.t += Put_found : int -> unit Effect.t
type _ Effect.t += Report : int -> unit Effect.t *)

let report_p n = printf "%d\n"(n);perform(Effects.E.Report n)
(* *Generates trace by printing the passed value, then performs the effect [Report] *)

(* let instrumentation comp x = match_with comp x
{ effc = (fun (type c) (eff: c Effect.t) ->
    match eff with
    | Put_found s -> Some (fun (k : (c,_) continuation) ->
            report_p(s); continue k ())
    | _ -> None
  );
  exnc = raise;
  retc = fun x-> x
} *)

(* let run_inst f x = instrumentation f x *)
let instrumentation () = fun () -> match_with (Modular_program.comp) ()
{ effc = (fun (type c) (eff: c Effect.t) ->
    match eff with
    | Effects.E.Put_found s -> Some (fun (k : (c,_) continuation) ->
            report_p(s); continue k ())
    | _ -> None
  );
  exnc = raise;
  retc = fun x-> x
}


let run_inst () =  instrumentation ()