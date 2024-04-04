open Utils
open Effect

let mem = ref 0
let put n = mem := n; perform(Utils.E.Put n)
(* *Modifies the value of [piece_of_memory] and performs the effect [Put_found] *)
let get () = !mem
(* *Dereferences [piece_of_memory] to return the value stored. *)

let comp () = put(5); put(6); put(-1); put(100)