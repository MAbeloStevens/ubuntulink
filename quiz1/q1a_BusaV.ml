(* Quiz 1 

   Student name 1: Vidhur Busannagari 
   Student name 2: Aarya Radhan

*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]

let rec rem_dups l =
  let rec helper seen result = function
    | [] -> List.rev result
    | h::t ->
        if List.mem h seen then
          helper seen result t
        else
          helper (h :: seen) (h :: result) t
  in
  helper [] [] l

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
      if List.mem h l2 then
        sub t l2
      else
        h :: sub t l2
    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | (a, b)::t ->
      if a = n then
        b :: outgoing_nodes t n
      else
        outgoing_nodes t n

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec nodes g =
  let rec extract_nodes g =
    match g with
    | [] -> []
    | (a, b)::t -> a :: b :: extract_nodes t
  in
  rem_dups (extract_nodes g)

(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)
let rec degree g =
  let rec max_degree nodes_list =
    match nodes_list with
    | [] -> 0
    | h::t ->
        let current_degree = List.length (outgoing_nodes g h) in
        let rest_max = max_degree t in
        if current_degree > rest_max then current_degree else rest_max
  in
  max_degree (nodes g)

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  | (a, b)::t ->
      if a = n || b = n then
        remove t n
      else
        (a, b) :: remove t n
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

  let rec reachable g n =
    let rec helper visited n =
      if List.mem n visited then
        visited
      else
        let neighbors = outgoing_nodes g n in
        List.fold_left helper (n :: visited) neighbors
    in
    rem_dups (helper [] n)