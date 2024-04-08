module E = struct
  type _ Effect.t += Put : int -> unit Effect.t

  let sum : int ref = ref 0
  (* To be used for the [sum_monitor] only. *)

  let flag : int ref = ref 2
  (* To be used for the [temp_monitor] only. *)
end