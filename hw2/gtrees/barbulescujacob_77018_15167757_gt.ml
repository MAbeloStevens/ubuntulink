(* Jacob Barbulescu *)
(* I pledge my Honor that I have abided by the Stevens Honor System. *)

type 'a gt = Node of 'a*(('a gt) list)

let t1 : int gt =Node (33 ,[ Node (12 ,[]) ;Node (77 ,[ Node (37 ,[ Node (14 , [])]) ;Node (48 , []) ;Node (103 , [])])])
let t2 : int gt = Node(45, [ Node (12, []); Node(33, [])])


(** Returns the max value in a list *)
let rec getMax : 'a list -> 'a -> 'a = fun list max ->
    match list with
    | [] -> max
    | h::t -> if h>max then getMax t h else getMax t max

(** Returns the height of a given tree *)
let rec height : 'a gt -> int = fun tree ->
    match tree with
    | Node(_, []) -> 1
    | Node(_, children) -> 1 + (getMax (List.map height children) 0)

(** Returns the sum of a list *)
let rec sumList : 'a list -> 'a = fun list ->
    match list with
    | [] -> 0
    | h::t -> h + sumList t

(** Returns the number of nodes in a tree *)
let rec size : 'a gt -> int = fun tree ->
    match tree with
    | Node(_, []) -> 1
    | Node(_, children) -> 1 + (sumList (List.map size children))

(** Prepends a given value to every list in a list of lists *)
let rec prepend : 'a list list -> 'a -> 'a list list = fun list value ->
    match list with
    | [] -> []
    | h::t -> (value::h)::(prepend t value)

let rec paths_to_leaves_helper : 'a gt -> int -> int list list = fun tree pathNum ->
    match tree with
    | Node(_, []) -> []
    | Node(value, h::t) -> (let paths = (paths_to_leaves_helper h 0) in if (List.length paths) = 0 then [[pathNum]] else (prepend (paths_to_leaves_helper h 0) pathNum))@(paths_to_leaves_helper (Node(value, t)) (pathNum+1))

(** Returns a list of the paths from the root to every leaf *)
let rec paths_to_leaves : 'a gt -> int list list = fun tree ->
    paths_to_leaves_helper tree 0

(** Determines if every value in a list is equal *)
let rec allEqual : 'a list -> bool = fun list ->
    match list with
    | [] -> true
    | [single] -> true
    | h1::h2::t -> if h1=h2 then allEqual (h2::t) else false

(** Determines if every leaf has the same depth *)
let is_leaf_perfect : 'a gt -> bool = fun tree ->
    allEqual (List.map List.length (paths_to_leaves tree))

(** Returns the preorder traversal of the tree *)
let rec preorder : 'a gt -> 'a list = fun tree ->
    match tree with
    | Node(value,[]) -> [value]
    | Node(value, children) -> value::(List.concat (List.map preorder children))

(** Returns the mirror image of a tree *)
let rec mirror : 'a gt -> 'a gt = fun tree ->
    match tree with
    | Node(value, []) -> Node(value, [])
    | Node(value, children) -> Node(value, (List.map mirror (List.rev children)))

(** Maps a given function to every value in the tree *)
let rec map : ('a -> 'b) -> 'a gt -> 'b gt = fun func tree ->
    match tree with
    | Node(value, []) -> Node(func value, [])
    | Node(value, children) -> Node(func value, (List.map (fun list -> map func list) children))

(** Folds the values of a tree based on a given function *)
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b = fun func tree ->
    match tree with
    | Node(value, []) -> func value []
    | Node(value, children) -> func value (List.map (fold func) children)

(** Returns the mirror of the tree *)
let mirror' : 'a gt -> 'a gt = fun tree ->
    fold (fun i rs -> Node(i, List.rev rs)) tree

let sum t = fold ( fun i rs -> i + List.fold_left ( fun i j -> i + j ) 0 rs ) t
let mem t e = fold ( fun i rs -> ((i=e) || (List.exists ( fun i -> i) rs)) ) t

(** Returns the maximum number of children that a node has *)
let degree : 'a gt -> int = fun tree ->
    (height tree)-1