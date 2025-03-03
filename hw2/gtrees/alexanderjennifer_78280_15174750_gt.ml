(*
   Jennifer Alexander
   I pledge my honor that I have abided by the Stevens Honor System. 
*)
type 'a gt = Node of 'a * ('a gt)list 

let t : int gt =
  Node (33 ,
        [ Node (12 ,[]) ;
          Node (77 ,
                [ Node (37 ,
                  [ Node (14 , [])]) ;
                Node (48 , []) ;
                Node (103 , [])])
        ])

        let t2 : int gt =
          Node (1, [])
        
        

   (*
      Given a general tree, process each node
      if current node has an empty list of children, return 0
      if children exist, use List.map to recursively iterate through 
      the nested lists, adding one each time to count the length of the paths
      use List.fold to apply max fucntion to this new list of path lengths
      and get a singular, maximum value as the result which represents the max path --> the height of the tree
      *)
let rec height (t:'a gt):int =
    match t with
    | Node(_, []) -> 1
    | Node(_,children) -> 
        1 + List.fold_left max 0 (List.map height children)

(*
Given a general tree, iterate through each node
If the current node has an empty list of children, return 1 to account for the node itself
If the current node has a non-empty list of children, add 1 and recursively 
iterate through the nested lists using list.fold_left to accumulate a single value
that counts each child/node recursively. 
*)
let rec size (t:'a gt):int = 
  match t with 
  |Node(_,[]) -> 1
  |Node(_,children) -> 
     1 + List.fold_left (fun acc child -> acc + size child) 0 children

(*
To generate all paths to leaves, encoded by the placement of the child (0,1,2...)
 If the tree/subtree is just a leaf, returns [[]]
Else, recursively process each child, keeping track of their index.
The helper function path_helper starts at index 0 and moves through the children
For each child, it first gets all its paths, then adds the child's index to each path,
and eventually combines the results with the rest of the children’s paths.
*)
let rec path_to_leaves (t:'a gt):(int list list) =
  match t with
  | Node(_, []) -> [[]]  (*no path here, return list of empty list*)
  | Node(_, children) -> 
      let rec path_helper index children = (*recursive function to help work with children*)
          match children with
          | [] -> [] (*no children, return empty list*)
          | child::t -> (*children exist, use recursion to process and generate paths*)
              let child_paths = path_to_leaves child in (*recursively get all paths through/to the children*)
              let indexed_paths = List.map (fun path -> index::path) child_paths in (**)
              indexed_paths @ path_helper (index + 1) t
          in path_helper 0 children

(*
   generate all paths to leaves and see if they are all of the same length
    by comparing each list length to the length of the first list.
   Use List.for_all to compare each list length and accumulate a single boolean value 

*)
let rec is_leaf_perfect (t:'a gt):(bool) =
  let paths = path_to_leaves t in 
  let l1 = List.length(List.hd paths) in
    match paths with
    | [] -> true
    | lists -> 
        List.for_all (fun lst -> List.length lst = l1) lists

(*
root, left, right 
access root node first.
If current node does not have any children, return a list with that the node value
If the current node has children, append the node value to the result and 
use List.map to recursively process the root nodes up until the leaf
use list.flatten to combine the results of mulitple lists 
*)
let rec preorder (t: 'a gt):(int list)=
      match t with 
      |Node(x,[]) -> [x]
      |Node(x,children) -> 
          x :: List.flatten (List.map preorder children);;

(*
approach: reverse the order of every list of children 
Base case: If current node has an empty list of children, return the node
Else: Use List.map to apply mirror recursively to each child, returning 
each Node with the reversal of its children list (using List.rev)
*)
let rec mirror (t:'a gt):('a gt) =
  match t with
  |Node(x,[]) -> Node(x,[])
  |Node(x, children) ->
    let mirroredC = List.map mirror children in
      Node(x, List.rev mirroredC)

(*
Encode map by processing each node in the tree
If the current node has no children, apply fucntion f to the value of the node and return the node
If the current node has a list of children, use List.map to recursively process the nested list of children,
while returning the Node with applying f to its value
*)
let rec map (f: 'a ->'b)(t:'a gt):('b gt)=
  match t with
  |Node(x,[]) -> Node(f x, [])
  |Node(x, children) ->
    Node (f x, List.map (map f) children);;


(*
To encode the recursion scheme on general trees, use List.map to 
process the nested lists of children. Apply fold (with the given function )
to each list of children recursively with list.map
*)
let rec fold (f: 'a -> 'blist -> 'b)(t: 'a gt):('b) =
  match t with 
  |Node(x,children) ->
    let results = List.map (fold f) children
  in f x results

(*
mirror with fold,
fold will recursively break down the tree to process from the leafs up
send fold a function that will return the current node but 
preserves its value, x, while reversing its children list to implement
the mirror effect 
*)
let rec mirror (t:'a gt):('a gt) =
  match t with
  |Node(x,[]) -> Node(x,[])
  |Node(x,children) -> 
      fold (fun x mirroredC -> Node(x, List.rev mirroredC)) t

(*
Find the maximum number of children that a node has in the given tree
The length of each node's children list represents its degree
As we recursively process the tree with List.map to generate a list of allDegrees, 
compare the current node's degree with the current maxDegree 
(current maxDegree is calculated by folding over the list of allDegrees 
and finding the current max, to ensure we are considering all subtrees)
*)
let rec degree (t:'a gt):(int) =
  match t with 
  |Node(x,[])-> 0
  |Node(x, children) ->
      let allDegrees = List.map degree children in
      let maxDegree = List.fold_left max 0 allDegrees
  in max(List.length children) maxDegree;;
