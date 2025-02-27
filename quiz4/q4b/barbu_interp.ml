(* Jacob Barbulescu *)
(* Newman Corriette *)
(* I pledge my Honor that I have abided by the Stevens Honor System *)

open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser

let rec sumList list =
  match list with 
  | [] -> 0
  | h::t -> h + (sumList t)

let sumResult result =
  match result with
  | Ok(list) -> Ok(sumList(list))

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Avg(es) ->
    match es with
    | [] -> error("avg: empty sequence")
    | _ ->
      (sumResult (eval_exprs es)) >>= fun sum ->
      return (sum/(List.length es))
  | _ -> failwith "Not implemented yet!"
and
  eval_exprs_helper list current =
  match list with
    | [] ->  Ok(current)
    | h::t -> (eval_expr h) >>= fun result ->
      (eval_exprs_helper t (result::current))
and
  eval_exprs : expr list -> (int list) result  =
  fun es ->
  eval_exprs_helper es []

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog



