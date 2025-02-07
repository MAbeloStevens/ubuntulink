(* Quiz 1 

   Student name 1: Payton Bates
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
    let rec helper t n out =
      match t with
      | [] -> out
      | (a,b)::tl ->
        if a = n (**a is the correct node**)
        then helper tl n (b::out) (**add its outgoing node to out**)
        else helper tl n out (**go next**)
    in helper t n [];;

(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)   
let rec nodes t =
  let rec helper t uniques =
    match t with
    | [] -> uniques
    | (a,b)::tl ->
      if List.mem a uniques (**a is a duplicate**)
      then if List.mem b uniques (**b is a duplicate**)
        then helper tl uniques  (**go next**)
        else helper tl (b::uniques) (**add b to uniques**)
      else if List.mem b uniques (**b is a duplicate**)
        then helper tl (a::uniques) (**add a to uniques**)
        else helper tl (b::a::uniques) (**add a and b to uniques**)
  in helper t [];;
    
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
  match t with
  | [] -> true
  | (a,b)::tl ->
    if List.length (outgoing_nodes t a) > 2 (**if there are more than 2 outgoing nodes**)
    then false (**the tree is not a binary tree**)
    else is_binary tl;; (**otherwise continue checking**)

(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                               

