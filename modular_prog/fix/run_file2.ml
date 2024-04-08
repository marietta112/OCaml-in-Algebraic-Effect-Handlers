open Monitor

let run () = Monitor.run_sum_mon ()

(* First it runs the computation against the instrumentation, then, the monitor takes the list
   of integers generated by Instr2 (mimicking the trace) and checks if a -1 is in the list. *)