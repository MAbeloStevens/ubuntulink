open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)
  | Avg(es) ->
    match es with
    | [] -> Error "avg: empty sequence"
    | Ok x ->
      let sum = List.fold_left (+) 0 x in
      return sum / List.length x
  | _ -> failwith "Not implemented yet!"
and
  eval_exprs : expr list -> (int list) result  =
  fun es ->
  match es with
  | [] ->  return []
  | h::t ->
    match eval_expr h with
    | Error e -> Error e
    | Ok e ->
      eval_exprs t >>= fun es -> return (e::es)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog