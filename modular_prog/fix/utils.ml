module E = struct
  (* Stores the total sum of integers. To be used for the [sum_monitor] only. *)
  let sum : int ref = ref 0
  
  (* Indicates whether an odd or event integer has been stored. 
     To be used for the [temp_monitor] only. *)
  let flag : int ref = ref 2

  (* Stores the previous integer. *)
  let prev : int ref = ref 0

  (* Variables *)
  let mem1 : int ref = ref min_int
  let mem2 : int ref = ref min_int
  let mem3 : int ref = ref min_int

  (* Hashtables are being used since they are mutable, extendible and provide efficient 
     insertion, deletion and lookups.*)
  (* Hashtable mapping references to their string counterparts. *)
  let names_hash : (string , int ref) Hashtbl.t = Hashtbl.create 10
  (* Hashtable mapping string of variable name to integer. *)
  let sums_hash : (string , int ref) Hashtbl.t = Hashtbl.create 10
  (* Hashtable mapping string of variable name to integer. *)
  let prev_hash : (string , int ref) Hashtbl.t = Hashtbl.create 10

  type _ Effect.t += Put : (string * int) -> unit Effect.t
  
  (* Effect that takes in a variable of type int ref and the unit, which returns an integer. *)
  type _ Effect.t += Get : string -> int Effect.t

  (* Effect that initialises a variable : int ref with a value int *)
  type _ Effect.t += Init : string * int -> unit Effect.t

  (* Exceptions raised for monitors. *)
  exception Invalid_value of int
  exception Exceeded_value of int 
  exception Not_alternating
  exception Inconsistent of string

let add_entry hashtbl_name first_entry second_entry = Hashtbl.add hashtbl_name first_entry second_entry 

let get_entry hashtbl_name first_entry = Hashtbl.find hashtbl_name first_entry

let update_entry hashtbl_name first_entry second_entry = let mem = get_entry hashtbl_name first_entry in mem := !second_entry
   (* Hashtbl.replace hashtbl_name first_entry second_entry *)

end