let mem = ref 0
let put n = mem := n
(* *Modifies the value of [piece_of_memory] *)
let get () = !mem
(* *Dereferences [piece_of_memory] to return the value stored. *)
let x = (ref 0)

let comp () = put(5); x:= get (); put(6); put(-1); put(100); print_int !x;;