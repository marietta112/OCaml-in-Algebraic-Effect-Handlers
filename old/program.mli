(* *An interface for the theory of STATE, which only uses the operations [put] and [get].
   *The types of the operations may be generalised to some abstract type [t].
*)

val put : int -> unit
(* *Takes an integer, stores it and returns the unit *)

val get : unit -> int
(* *Takes a unit, returns the integer stored in memory *)

val report_p : int -> unit
(* *Given a (-1), it reports a problem to the console, else, it prints the integer given. *)

val mon : unit -> unit
(* *Handles a computation by printing the value stored in memory using a handler *)
