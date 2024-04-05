open Instr
open Monitor
open Modular_program
open Analyser

let run () = Monitor.run_mon (Instr.run_inst ()) (* only run this code if you are using Instr and Monitor *)
(* let run () = Analyser.run_analyser () *) (* only run this code if you are using Analyser *)