(* Quiz 1 

   Student name 1: Aidan Brown
   Student name 2: Colm Murrer

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

let rec mem e l =
  match l with
  | [] -> false
  | h::t -> (h=e) || mem e t

let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t ->
    if mem h l2
    then sub t l2
    else h:: sub t l2
    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | h::t ->
    if fst h = n
    then snd h :: outgoing_nodes t n
    else outgoing_nodes t n

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)

(** let rec node_list g l =
  match g with
  | [] -> []
  | h::t -> l @ [fst h] @ [snd h] @ node_list t l

let rec remove_dups l =
  match l with
  | [] -> []
  | h::t ->
    if mem h t
    then remove_dups t
    else h :: remove_dups l

let rec nodes g =
  remove_dups (node_list g [])
*)

(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)
let rec degree g =
  failwith "implement"

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  | h::t ->
    if fst h = n
    then remove t n
    else if snd h = n
    then remove t n
    else h :: remove t n
 
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith "implement"

