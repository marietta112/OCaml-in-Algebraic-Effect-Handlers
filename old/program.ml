(* *Implementation file for program.mli. 
   *Defines the behvaiour for [put] and [get], that act on [piece_of_memory].  
*)

open Effect
open Effect.Deep
open Printf

type _ Effect.t += Put_found : int -> unit Effect.t
type _ Effect.t += Report : int -> unit Effect.t

let piece_of_memory = ref 0;;

let put n = piece_of_memory := n; perform(Put_found n);;
(* *Modifies the value of [piece_of_memory] and performs the effect [Put_found] *)
let get () = !piece_of_memory;;
(* *Dereferences [piece_of_memory] to return the value stored. *)
let report_p n = perform(Report n);;
(* *It performs the effect [Report] if the program attempts to store (-1) in memory. *)

let mon () = 
  let comp () = put(5); put(6); put(-1); put(100) in
  let instrumentation () = match_with comp ()
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Put_found s -> Some (fun (k : (c,_) continuation) ->
              report_p(s); continue k ())
      | _ -> None
    );
    exnc = raise;
    retc = fun x-> x
  } in
  match_with instrumentation () {
    effc = (fun (type b) (eff: b Effect.t) ->
      match eff with
      | Report n -> Some (fun (k: (b,_) continuation) -> 
        if n = (-1) then printf "Put with value -1 encountered.\n" else printf "Put found with value: %d\n" (n); continue k ())
      | _ -> None
    );
    exnc = raise;
    retc = fun x-> x
  }