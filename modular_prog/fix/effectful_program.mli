open Utils
val put : Utils.E.var -> int -> unit
(* *Takes an integer, stores it in a single memory location and returns the unit *)

val get : Utils.E.var -> int
(* *Takes a unit, returns the integer stored in memory *)

val init : Utils.E.var -> int -> unit

val main : unit -> unit
(* *Program using [put] and [get] *)