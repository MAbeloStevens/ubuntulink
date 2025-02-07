(* Quiz 1 

   Student name 1: Christopher Bernard
   Student name 2:

*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec subtract l1 v = 
  match l1 with
  | [] -> l1
  | h::t ->
    if (h - v) > 0
    then (h-v) :: subtract t v
    else subtract t v

let rec sub l1 l2 =
  match l2 with
  | [] -> l1
  | h::t -> sub (subtract l1 h) (t)
  
    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | (y, z)::t -> 
    if (y = n)
    then z :: outgoing_nodes t n
    else outgoing_nodes t n

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
(*
let rec nodes g =
  match g with
  | [] -> []
  | (y, z)::t -> 
    if (y = 1)
    then z :: outgoing_nodes t 1
    else outgoing_nodes t 1
*)
(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)
(*
let rec degree g =
  failwith "implement"
*)
(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
(*
let rec remove g n =
  failwith "implement"
*)
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)
(*
let rec reachable g n =
  failwith "implement"
*)
