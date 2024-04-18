module E = struct
  (* type _ Effect.t += Put : int -> unit Effect.t *)
  type _ Effect.t += Put : int ref * int -> unit Effect.t
  
  (* Effect that takes in a variable of type int ref and the unit, which returns an integer. *)
  type _ Effect.t += Get : int ref -> int Effect.t

  (* Effect that initialises a variable : int ref with a value int *)
  type _ Effect.t += Init : int ref * int -> unit Effect.t

  (* Stores the total sum of integers. To be used for the [sum_monitor] only. *)
  let sum : int ref = ref 0
  
  (* Indicates whether an odd or event integer has been stored. 
     To be used for the [temp_monitor] only. *)
  let flag : int ref = ref 2

  (* Stores the previous integer. *)
  let prev : int ref = ref 0

  let mem1 : int ref = ref 0
  let mem2 : int ref = ref 0
  let mem3 : int ref = ref 0
  let mem4 : int ref = ref 0
  let mem5 : int ref = ref 0

  (* Exceptions raised for monitors. *)
  exception Invalid_value of int
  exception Exceeded_value of int 
  exception Not_alternating
  exception Inconsistent
end