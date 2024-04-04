open Effect
open Effect.Deep
open Printf
open Instr
open Utils
open String
open List

(* let flag = ref false

let func () =
for i = 1 to (String.length !(Utils.E.trace) - 1) do
  print_char !(Utils.E.trace).[i];
  perform(Utils.E.Report !(Utils.E.trace).[i])
done
(*let func () = perform(Utils.E.Report Utils.trace) in String.iter func Utils.trace*)

let monitor x = match_with func x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Report s -> 
      Some (fun (k: (b,_) continuation) -> 
      print_string !(Utils.E.trace);
      if s = ('1') && !flag then printf "Put with value -1 encountered.\n" 
      else if s = ('-') then flag := true
                    else printf "Put found with allowed value %c.\n" s; continue k ())
    | _ -> None
  );
  (* If an effect is performed by [inst], then we have the type of the raised effect and its
     continuation k. If the types match, then we can evaluate the handler's code. Otherwise, 
     an exception is raised. *)
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
} *)


let func2 () =
  for i = 0 to (List.length !(Utils.E.trace_lst) - 1) do
    perform(Utils.E.Report (List.nth !(Utils.E.trace_lst) i))
  done

let monitor2 x = match_with func2 x
{
  effc = (fun (type b) (eff: b Effect.t) ->
    match eff with
    | Utils.E.Report s -> 
      Some (fun (k: (b,_) continuation) -> 
      if s = (-1)  then printf "Put with value -1 encountered.\n" 
                    else printf "Put found with allowed value %d.\n" s; continue k ())
    | _ -> None
  );
  (* If an effect is performed by [inst], then we have the type of the raised effect and its
     continuation k. If the types match, then we can evaluate the handler's code. Otherwise, 
     an exception is raised. *)
  exnc = raise; (* Optional *)
  retc = fun x-> x (* Required *)
}

let run_mon2 () = monitor2 ()