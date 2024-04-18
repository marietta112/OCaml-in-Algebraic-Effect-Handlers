val put : int ref -> int -> unit
(* *Takes an integer, stores it in a single memory location and returns the unit *)

val get : int ref -> int
(* *Takes a unit, returns the integer stored in memory *)

val init : int ref -> int -> unit

val main : unit -> unit
(* *Program using [put] and [get] *)