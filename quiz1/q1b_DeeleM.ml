(* Quiz 1 

   Student name 1: Mathijs Deelen
   Student name 2:
*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]
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
let rec outgoing_nodes t (n:int) = 
  match t with
  |[] -> []
  |(p,c)::xs ->
      if p = n then
        c::(outgoing_nodes xs n)
      else 
        outgoing_nodes xs n;; 

  
    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)   
let rec nodes t = 
  match t with
  |[] -> []
  |(p,c)::xs -> c :: nodes xs
  
    
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

                               
