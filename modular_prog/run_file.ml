open Instr
open Monitor
open Modular_program
open Analyser

(* let run () = Monitor.run_mon (Instr.run_inst ()) *)
let run () = Analyser.run_analyser ()