open Utils
open Effect

let mem = ref 0
let put n = mem := n; perform(Utils.E.Put n)
(* *Modifies the value of [mem] and performs the effect [Put_found] *)
let get () = !mem
(* *Dereferences [mem] to return the value stored. *)

let comp () = put(5); put(6); put(-1); put(100)