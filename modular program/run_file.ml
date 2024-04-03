open Instr
open Monitor
open Modular_program

let run () = Monitor.run_mon (Instr.run_inst ())