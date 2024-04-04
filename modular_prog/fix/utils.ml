module E = struct
  type _ Effect.t += Put : int -> unit Effect.t
  (* type _ Effect.t += Report : char -> unit Effect.t *)
  type _ Effect.t += Report : int -> unit Effect.t

  let trace : string ref = ref ""
  let trace_lst : int list ref = ref [] 
end