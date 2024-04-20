module E = struct
  (* Stores the total sum of integers. To be used for the [sum_monitor] only. *)
  let sum : int ref = ref 0
  
  (* Indicates whether an odd or event integer has been stored. 
     To be used for the [temp_monitor] only. *)
  let flag : int ref = ref 2

  (* Stores the previous integer. *)
  let prev : int ref = ref 0

  (* Record for variables to keep track of their individual running total. *)
  type var = { mutable value : int ; mutable total : int; mutable prev : int }

  (* Variables *)
  let mem1 : var = {value = min_int; total = 0; prev = 0}
  let mem2 : var = {value = min_int; total = 0; prev = 0}
  let mem3 : var = {value = min_int; total = 0; prev = 0}
  let mem4 : var = {value = min_int; total = 0; prev = 0}
  let mem5 : var = {value = min_int; total = 0; prev = 0}

  (* type _ Effect.t += Put : int -> unit Effect.t *)
  type _ Effect.t += Put : var * int -> unit Effect.t
  
  (* Effect that takes in a variable of type int ref and the unit, which returns an integer. *)
  type _ Effect.t += Get : var -> int Effect.t

  (* Effect that initialises a variable : int ref with a value int *)
  type _ Effect.t += Init : var * int -> unit Effect.t

  type _ Effect.t += Alias : var * var -> unit Effect.t

  (* Exceptions raised for monitors. *)
  exception Invalid_value of int
  exception Exceeded_value of int 
  exception Not_alternating
  exception Inconsistent
  exception Alias_Error
end