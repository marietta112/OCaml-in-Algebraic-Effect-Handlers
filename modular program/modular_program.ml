open Instr
open Monitor
open Effect
open Effect.Deep

type _ Effect.t += Put_found : int -> unit Effect.t

let piece_of_memory = ref 0

let put n = piece_of_memory := n; perform(Put_found n)
(* *Modifies the value of [piece_of_memory] and performs the effect [Put_found] *)
let get () = !piece_of_memory
(* *Dereferences [piece_of_memory] to return the value stored. *)

let comp () = put(5); put(6); put(-1); put(100)

(* Monitor.run_mon (Instr.run_inst comp ()) ()  *)
(* ^ The problem with this: produces an error on compilation as follows: 
   File "modular_program.ml", line 17, characters 16-40:
    17 | Monitor.run_mon (Instr.run_inst comp ()) ()
                     ^^^^^^^^^^^^^^^^^^^^^^^^
    Error: This expression has type Instr.r
       but an expression was expected of type unit -> Monitor.t


  Note that OCaml has a call-by-value semantics, thus the first argument passed to run_mon is
  evaluated down to a value before passed as an argument to run_mon, which is not what we want.
*)
