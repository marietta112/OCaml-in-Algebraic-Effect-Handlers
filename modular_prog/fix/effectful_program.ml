open Utils
open Effect

(* Variable that acts as a piece of memory to be updated and read from. It has the value of 
   [prev] for the case when [get'] if the first operation in [main]. *)
let mem = ref !(Utils.E.prev)

(* Modifies the value of [mem] and performs the effect [Put]. *)
let put n = mem := n; perform(Utils.E.Put n)

(* Dereferences [mem] to return the value stored. *)
let get () = !mem

(* Acts like [get] but it raises the effect [Get] after dereferencing. *)
let get' () = !mem; perform(Utils.E.Get (mem, ()))

(* [x] is a temporary variable in the expression [comp], which is a sequence of [put] and [get]. *)
let main () = let x = (ref 0) in 
              let comp () = put(199); x := get' (); print_endline ""; 
              put(200); put(110); put(10); x := get' (); print_endline ""
              in comp ()