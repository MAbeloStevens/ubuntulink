
(* Quiz 1 
   Student name 1: Sahana Ali
   Student name 2: Nicole Mak
*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

(* Sample Tree *)

(*
      12
      /\ 
     /  \  
    7   43
   /    /\ 
  /    /  \  
 4    33  77
*)
         
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)
(* Returns the list of nodes outgoing from node n in tree t *)
(*THis code doesn't compile*)
let rec outgoing_nodes t (n:int) =
  match t with 
  | [] -> []   (*empty elist *)
  | (p, c) :: remaining -> 
      if p = n then  c :: outgoing_nodes remaining n 
      else  outgoing_nodes remaining  n 

let rec nodes t =
  (* extracts all unique nodes from the tree *)
  match t with
  | [] -> []
  | (p, c) :: remaining -> 
      rem_dups (p :: c :: nodes remaining)

    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]
*)
let rec mem e l = 
  match l with 
  | [] -> false 
  | h :: t -> (h=e) || mem e t 
let rec rem_dups l = 
  match l with 
  | [] -> []
  | h :: t -> 
    if mem h t 
    then rem_dups t
    else h :: rem_dups t 
let rec nodes t =
  if t = [] then 
    failwith "complete"
else 
  rem_dups t 

    
(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec leaves t =
  failwith "complete"

(* 
   Returns the root of a tree
   Eg. root ex =>  [12]
*)
let rec root t =
  failwith "complete"

(* 
   Returns the boolean indicating if the tree is a binary tree.
   Eg. is_binary ex =>  true
*)
let rec is_binary t =
  failwith "complete"

(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                            