open Instr2
open Monitor2
open Modular_program
open Utils

let run1 () = Instr2.run_inst2 ()
let run2 () = Monitor2.run_mon2 ()

let main () = run1 (); run2 ()