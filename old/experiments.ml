open Effect
open Effect.Deep

type _ Effect.t += Num: int -> int Effect.t
exception Code of int

let prog () = perform(Num 0); raise (Code 0)

let handle_prog = try_with prog ()
{ effc = (fun (eff: int t) ->
    match eff with
    | Num n -> Some (fun (k: (int, _) continuation) ->
        continue k (n+1))
    | _ -> None); 
};;

(* Even though we know the type of the effect, when not using locally abstract types, 
   the compiler raises an error that the types given are not general. 
   Error: This field value has type
         int Effect.t -> ((int, 'a) Effect.Deep.continuation -> 'a) option
       which is less general than
         'b. 'b Effect.t -> (('b, 'c) Effect.Deep.continuation -> 'c) option
*)