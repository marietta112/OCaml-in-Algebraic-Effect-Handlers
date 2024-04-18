open Utils
open Effect

(*

(* Variable that acts as a piece of memory to be updated and read from. It has the value of 
   [prev] for the case when [get'] if the first operation in [main]. *)
let mem = ref !(Utils.E.prev)

(* Modifies the value of [mem] and performs the effect [Put]. *)
let put n = (mem := n); perform(Utils.E.Put n)

(* Dereferences [mem] to return the value stored and raises the effect [Get] after dereferencing.*)
let get () = !mem; perform(Utils.E.Get mem)

(* [x] is a temporary variable in the expression [comp], which is a sequence of [put] and [get]. *)
let main () = let x = (ref 0) in 
              let comp () = put(400); x := get (); print_int !x; print_endline "";
              put(-1); print_endline ""; put(110); print_endline ""; put(10); 
              x := get (); print_endline ""
              in comp ()
*)
let init loc n = perform(Utils.E.Init (loc,n))


let put loc n = (loc := n); perform(Utils.E.Put (loc, n))


let get loc = !loc; perform(Utils.E.Get loc)

let main () = let x = (ref 0) in 
              let comp () = init Utils.E.mem1 0; init Utils.E.mem2 !Utils.E.prev; 
                            put Utils.E.mem1 400; put Utils.E.mem2 (-1); put Utils.E.mem1 400; 
                            print_int !Utils.E.mem1; print_endline ""; 
                            print_int !Utils.E.mem2; print_endline ""
              in comp ()