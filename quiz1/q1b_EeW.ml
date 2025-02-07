(* Quiz 1 

   Student name 1: William Ee
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
  List.fold_left (fun a (p, c) -> if p = n then c :: a else a) [] t
    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)   
let rec nodes t =
  let no_dupe lst =
    List.fold_left (fun a i -> if List.mem i a then a else i :: a) [] lst in
    no_dupe (List.fold_left (fun a (p, c) -> p :: c :: a) [] t)
    
(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec leaves t =
  let all = nodes t in
  let parents = List.fold_left (fun a (p, _) -> p :: a) [] t in
  List.filter (fun n -> not (List.mem n parents)) all

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
  let rec count n =
    List.length (outgoing_nodes t n)
  in
  List.for_all (fun n -> count n <= 2) (nodes t)

(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"
