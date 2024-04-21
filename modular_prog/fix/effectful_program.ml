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
let init var n = perform(Utils.E.Init (var,n))

(* var is a record with its first entry being its value. *)
let put (var: Utils.E.var ref) (n: int) = (!var.value <- n); perform(Utils.E.Put (var, n))


let get (var: Utils.E.var ref) = !var.value; perform(Utils.E.Get var)


let alias (x: Utils.E.var ref) (y: Utils.E.var ref) = (x := !y); perform (Utils.E.Alias (x,y))

let check (x: Utils.E.var ref) (y: Utils.E.var ref) = perform(Utils.E.Check (x,y))

(* let main' () = let x = (ref 0) in 
              let comp () = init Utils.E.mem1 0; init Utils.E.mem2 !Utils.E.prev; 
                            put Utils.E.mem1 400; x:= get Utils.E.mem1; put Utils.E.mem2 (-1); put Utils.E.mem1 400; 
                            print_int Utils.E.mem1.value; print_endline ""; 
                            print_int Utils.E.mem2.value; print_endline ""
              in comp () *)

let main () = let comp () = init Utils.E.mem1 50; init Utils.E.mem2 10; alias Utils.E.mem1 Utils.E.mem2;
                              print_int (!Utils.E.mem1).value; print_endline ""; print_int (!Utils.E.mem2).value; print_endline "";
                              put Utils.E.mem2 501; check Utils.E.mem1 Utils.E.mem2; put Utils.E.mem2 (-1)
               in comp ()