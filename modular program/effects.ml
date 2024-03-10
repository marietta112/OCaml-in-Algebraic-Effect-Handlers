module E = struct
  type _ Effect.t += Put_found : int -> unit Effect.t
  type _ Effect.t += Report : int -> unit Effect.t
end

(* *Effect constructors must be shared with the other layers, otherwise, they are not the 
   same constructor! *)