(*
  GENERAL TREES
  Author: Iva Buric
  Date: 02/23/2025
  I pledge my honor that I have abided by the Stevens Honor System.
*)

(* Trees are assumed to be non-empty *)
(* Data Type of General Trees *)
type 'a gt = Node of 'a*('a gt) list

(* Visual Representation of the Tree *)
(*
      33
     /  \
   12    77
       /  |  \
     37  48   103  
     |
     14 
*)
let t1: int gt =
  Node (33,
      [Node (12 ,[]);
       Node (77,
            [Node (37, [Node (14, [])]);
            Node (48, []);
            Node (103, [])])
        ])

(*
          33
     /  |   |   |  \ 
   12   32  82  10  162 
*)
let t2: int gt =
  Node (33,
      [Node (12 ,[]);
      Node (32 ,[]);
      Node (82 ,[]);
      Node (10 ,[]);
      Node (162 ,[]);
      ])

(*
      33
     /  \
   48    77
 /  |      \
37  48     205   
    |
    14
*)
let t3: int gt =
  Node(33,
      [Node(48,[Node(37, []); Node(48,[Node(14,[])])]);
      Node (77,[Node(205,[])])
      ])

(*
      33
     /  \
   48    48
 /  |    |  \ 
37  48   48  37 
    |    |
    14   14
*)
let t4: int gt = 
  Node(33, 
    [Node(48, [Node(37, []);
              Node(48, [Node(14,[])])]);
    Node(48, [Node(48, [Node(14,[])]);
            Node(37,[])])
    ])

(*  

1
|
2
|
3
|
4
|
5

*)
let t5: int gt = 
  Node(1, [
    Node(2, [
      Node(3, [
        Node(4, [
          Node(5, [])
        ])
      ])
    ])
  ])

(* Given n builds a general tree that is a leaf holding n as data *)
let mk_leaf : 'a -> 'a gt = 
  fun n -> 
    Node(n, [])

(* Given a gt returns its height *)
let rec height t = 
  match t with
  | Node(n, []) -> 1 
  | Node(n, subt) -> 1 + List.fold_left(max) 0 (List.map height subt)
  
(* Helper function that computes the sum of a list *)
  let rec suml l = 
  match l with 
  | [] -> 0
  | h::t -> h + suml t

(* Given a gt returns its size *)
let rec size t = 
  match t with
 | Node(n, subt) -> 1 + suml(List.map size subt)

(* Helper function to get the number of children of a node *)
let num_children (Node(n, subt)) = List.length subt

(* Helper function that computes the path for each node recursively *)
let rec helper node path =
  match node with
  | Node (n, []) -> [path]  
  | Node (n, subt) -> List.flatten(
    List.mapi(fun i current -> helper current (path @ [i])) subt
    )

(* Main function to compute paths to leaves of a given gt *)
let paths_to_leaves t = helper t []

(* Determines whether all leaves of gt have the same depth *)
let rec is_leaf_perfect t =
  match t with
  | Node(n, []) -> true  
  | Node(n, subt) -> let h = height(List.hd subt) in
      List.for_all(fun st -> height st = h && is_leaf_perfect st) subt

(* Given a gt returns its preorder traversal *)
let rec preorder t =
  match t with 
  | Node(n, []) -> [n] 
  | Node(n, subt) -> n::List.flatten(List.map preorder subt) 

(* Given a gt returns its mirror image *)
let rec mirror t = 
  match t with 
  | Node (n, subt) -> let newt = List.rev(List.map mirror subt) in Node (n,newt)

(* Produces a gt resulting from t by map *)
let rec map f t = 
  match t with
  | Node(n, subt) -> let nodes = List.map(map f) subt in Node(f n,nodes)

(* Enccodes the recursion scheme over gts *)
let rec fold f t = 
  match t with
  | Node(n, subt) -> let nodes = List.map(fold f) subt in f n nodes

(* Second version of mirrror using fold *)
let rec mirror' t = fold(fun n subt -> Node(n, List.rev subt)) t

(* Given a gt returns the maximum number of children that a node has *)
let rec degree t =
  match t with
  | Node (n, subt) -> let max_degree = List.fold_left(fun acc node -> max acc (degree node)) 0 subt 
  in max (num_children t) max_degree
