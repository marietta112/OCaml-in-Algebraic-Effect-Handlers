module E = struct
  type _ Effect.t += Put : int -> unit Effect.t
  
  (* Effect that takes in a variable of type int ref and the unit, which returns an integer. *)
  type _ Effect.t += Get : (int ref) * unit -> int Effect.t

  (* Stores the total sum of integers. To be used for the [sum_monitor] only. *)
  let sum : int ref = ref 0
  
  (* Indicates whether an odd or event integer has been stored. 
     To be used for the [temp_monitor] only. *)
  let flag : int ref = ref 2

  (* Stores the previous integer. *)
  let prev : int ref = ref 0
end