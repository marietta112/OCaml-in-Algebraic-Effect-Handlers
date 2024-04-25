open Utils
val put : string -> int -> unit
(* *Takes an integer, stores it in a single memory location and returns the unit *)

val get : string -> int
(* *Takes a unit, returns the integer stored in memory *)

val init : string -> int -> unit

val main : unit -> unit
(* *Program using [put] and [get] *)