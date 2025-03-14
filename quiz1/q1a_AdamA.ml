(* Quiz 1 

   Student name 1: Avery Adamo
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
let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t ->
    if List.mem h l2
    then sub t l2
    else h :: sub t l2
    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)

let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | (x, y)::t ->
    if x=n
    then y :: (outgoing_nodes t n)
    else x
  
  failwith "implement"




(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
 *)

let rec nodes_helper g c =
  match g with
  | [] -> []
  | (x, y)::t -> 
    if List.mem x c = false
    then nodes_helper g (x :: c)
    else (if List.mem y c = false then nodes_helper g (y :: c) else nodes_helper g c)


(**
let nodes g = 
    


  failwith "implement"

(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)

let count_outgoing_nodes e g =
  match g with
  | [] -> 0
  | (x, y)::t ->
    if x=e then 1 + count_outgoing_nodes e t
    else count_outgoing_nodes e t

let rec degree g =
  


  failwith "implement"

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  failwith "implement"
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith "implement"
*)
 

