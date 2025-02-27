open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
(** Brayden Abo and Ian Nevins *)

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
  | Avg es ->
      (match eval_exprs es with
       | Ok [] -> Error "avg: empty sequence"
       | Ok values -> Ok (List.fold_left (+) 0 values / List.length values)
       | Error msg -> Error msg)
  | _ -> failwith "Not implemented yet!"

and eval_exprs : expr list -> (int list) result =
  fun es ->
  match es with
  | [] -> Ok []
  | h::t -> 
      (match eval_expr h, eval_exprs t with
       | Ok v, Ok vs -> Ok (v :: vs)
       | Error msg, _ | _, Error msg -> Error msg)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog



