(* Quiz 1 

   Student name 1: Jaran Binning
   Student name 2:
*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

(* Sample Tree *)

let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]

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
  | [] -> []
  | hd::tl -> if (fst hd) = n
              then (snd hd) :: (outgoing_nodes tl n)
              else outgoing_nodes tl n
    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*) 
let rec node_helper t =
  match t with
  | [] -> []
  | hd::tl -> fst hd :: snd hd :: node_helper tl

let rec remove_dupe l =
  match l with
  | [] -> []
  | hd::tl -> if List.mem hd tl 
                then remove_dupe tl 
                else hd::remove_dupe tl
let rec nodes t =
  remove_dupe (node_helper t)
              
    
(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec leaves t =
  match t with
  | [] -> []
  | hd::tl -> if List.length (outgoing_nodes t (snd hd)) = 0
     then (snd hd) :: leaves tl 
     else leaves tl

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
let rec is_binary_helper t nl =
  match nl with 
  | [] -> true
  | hd::tl -> if List.length (outgoing_nodes t hd) > 2
                then false
                else is_binary_helper t tl

let rec is_binary t =
  let node_list = nodes t in
  is_binary_helper t node_list


(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                               

