(* Quiz 1 

   Student name 1: Sarah Basil

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
       
(* sub helper: removes all occurrence of x in l *)
let rec remove_all x l =
  match l with
  | [] -> []
  | h::t ->
      if (h=x) then remove_all x t  (* skip h and keep removing*)
      else h::remove_all x t



(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec sub l1 l2 =
  match l2 with
  | [] -> l1  (* base case: no elements to remove *)
  | h::t ->
      let l1' = remove_all h l1 in   (*remove 1st x*)
      sub l1' t    (*continue with rest*)



    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> [] (*empty list base case*)
  | (u,v)::rest ->
      if (u=n) then v::outgoing_nodes rest n (*prepend if its a neighbor*)
      else outgoing_nodes rest n



(*nodes helper functions: *)

(*checks if x is in l:*)
let rec present x l =
  match l with
  | [] -> false (*not present*)
  | h::t -> (h=x) || present x t (*overall boolean result*)

let add_unique x lst =
  if present x lst then lst
  else x::lst (*ONLY add x if its UNIQUE and not present already*)
      

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)

let rec nodes g =
  match g with
  | [] -> []
  | (u,v)::rest ->
      let tail_nodes = nodes rest in
      (* u and v but no duplicates -- recursively through the rest: *)
      let with_u = add_unique u tail_nodes in
      let with_v = add_unique v with_u in
      with_v

(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)

(*degree helpers:*)
let rec max a b = if a > b then a else b (*simple max func*)

(*max in the list:*)
let rec list_max xs =
  match xs with
  | [] -> 0
  | h::t -> max h (list_max t)

let rec degree g =
  let nds = nodes g in
  (* for each node n --> get outgoing edges, collect in a list *)
  let degrees = List.map (fun n -> List.length (outgoing_nodes g n)) nds in
  list_max degrees


(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  | (u, v)::rest ->
      if (u=n) || (v=n) then
        remove rest n  (* skip this edge *)
      else
        (u,v)::remove rest n
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith "implement"

