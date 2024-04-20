open Utils
val put : Utils.E.var ref -> int -> unit
(* *Takes an integer, stores it in a single memory location and returns the unit *)

val get : Utils.E.var ref-> int
(* *Takes a unit, returns the integer stored in memory *)

val init : Utils.E.var ref -> int -> unit

val alias : Utils.E.var ref-> Utils.E.var ref-> unit

val main : unit -> unit
(* *Program using [put] and [get] *)