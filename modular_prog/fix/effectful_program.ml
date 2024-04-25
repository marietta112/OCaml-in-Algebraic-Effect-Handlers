open Utils
open Effect

let init var n = perform(Utils.E.Init (var,n))

let put var n = Utils.E.update_entry Utils.E.names_hash var (ref n); perform(Utils.E.Put (var, n))

let get var = let x = Utils.E.get_entry (Utils.E.names_hash) (var) in !x; perform(Utils.E.Get var)

let init_names_hash () = Utils.E.add_entry Utils.E.names_hash "mem1" Utils.E.mem1; 
                    Utils.E.add_entry Utils.E.names_hash "mem2" Utils.E.mem2; 
                    Utils.E.add_entry Utils.E.names_hash "mem3" Utils.E.mem3

let init_sums_hash () = Utils.E.add_entry Utils.E.sums_hash "mem1" (ref 0); 
                          Utils.E.add_entry Utils.E.sums_hash "mem2" (ref 0); 
                          Utils.E.add_entry Utils.E.sums_hash "mem3" (ref 0)

let init_prev_hash () = Utils.E.add_entry Utils.E.prev_hash "mem1" (ref 0); 
                          Utils.E.add_entry Utils.E.prev_hash "mem2" (ref 0); 
                          Utils.E.add_entry Utils.E.prev_hash "mem3" (ref 0)

let main () = let x = (ref 0) in 
              let comp () = init_names_hash (); init_sums_hash (); init_prev_hash (); init "mem1" 0; init "mem2" 15; 
                            put "mem1" 400; x:= get "mem1"; put "mem2" (-1); put "mem1" 400; 
                            print_int !Utils.E.mem1; print_endline ""; 
                            print_int !Utils.E.mem2; print_endline ""
              in comp ()