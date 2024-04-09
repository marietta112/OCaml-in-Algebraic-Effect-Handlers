(* A monitor that checks whether the program tried to store a -1 in memory. *)
val run_mon1 : unit -> unit

(* A monitor that checks that the running total of the integers stored in memory is less than 500. *)
val run_sum_mon : unit -> unit

(* A monitor that checks that integers stored in memory alternate between odd and even. *)
val run_temporal_mon : unit -> unit

(* A monitor that stores the previous integer stored in memory and checks whether the [get] operation
   returns the most recent integer stored in memory. *)
val run_mon2 : unit  -> unit

(* A monitor that checks that the running total of the integers stored in memory is less than 500
   and checks whether the [get] operation returns the most recent integer stored in memory. *)
val run_mon3 : unit  -> unit