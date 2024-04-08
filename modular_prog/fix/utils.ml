module E = struct
  type _ Effect.t += Put : int -> unit Effect.t

  let sum : int ref = ref 0
  (* To be used for the [sum_monitor] only. *)
end