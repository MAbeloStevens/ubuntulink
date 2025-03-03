type 'a gt = Empty | Node of 'a*('a gt) list (*Create a gt type*)

let t : int gt = (*Create gt variable t*)
  Node (33,
        [Node (12, []);
         Node (77,
               [Node (37,
                      [Node (14, [])]);
                Node (48, []);
                Node (103, [])])
        ])

(*Get height of gt by iterating through each child and adding to a max count until we reach an empty subtree*)
let rec height : 'a gt -> int = 
  fun t ->
    match t with
    | Empty -> 0
    | Node(_, children) -> 1 + List.fold_left (fun total c -> max total (height c)) 0 children

(*Get the size of gt by iterating through each child an adding to a total count until we reach an empty subtree*)
let rec size : 'a gt -> int = 
  fun t ->
    match t with
    | Empty -> 0
    | Node(_, children) -> 1 + List.fold_left (fun total c -> total + size c) 0 children

(*Get the path to to leaves by using numbers to determine which child you visited (0 is left most, then 1, ...) and turn them into a list*)
let rec path_to_leaves : 'a gt -> int list list = 
  fun t ->
    match t with 
    | Empty -> []
    | Node(_, []) -> [[]]
    | Node(_, children) -> List.flatten(List.mapi (fun idx c -> List.map (fun path -> idx :: path) (path_to_leaves c)) children)

(*Helper function to get depths of a node*)
let rec get_depths : 'a gt -> int -> int list = 
    fun t depth ->
      match t with
      | Empty -> []
      | Node(_, []) -> [depth]
      | Node(_, children) -> List.flatten(List.map (fun c -> get_depths c (depth + 1)) children)

(*Check if all leaves have the same depth by checking if all the elements returned by get_depths are equal to each other*)
let rec is_leaf_perfect : 'a gt -> bool = 
  fun t ->
    match get_depths t 0 with
    | [] -> true
    | depths -> List.for_all (fun x -> x = List.hd depths) (List.tl depths)

(*List nodes in preorder*)
let rec preorder : 'a gt -> 'a list = 
  fun t ->
    match t with
    | Empty -> []
    | Node(curr, children) -> [curr] @ List.flatten(List.map preorder children)

(*Mirror the gt*)
let rec mirror : 'a gt -> 'a gt = 
  fun t ->
    match t with
    | Empty -> Empty
    | Node(curr, children) -> Node(curr, List.rev (List.map mirror children))

(*Map function that allows a function to be applied to gt*)
let rec map : ('a -> 'b) -> 'a gt -> 'b gt = 
  fun f t ->
    match t with
    | Empty -> Empty
    | Node(curr, children) -> Node(f curr, List.map (map f) children)

(*Fold function*)
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
  fun f t ->
    match t with 
    | Empty -> failwith "gt must be non-empty"
    | Node(curr, children) -> f curr (List.map (fold f) children)

(* Check if fold works*)
let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i + j) 0 rs) t

let mem t e = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(*Mirror using fold*)
let mirror' : 'a gt -> 'a gt = 
  fold (fun curr children -> Node(curr, List.rev children))

(*Get the maximum number of children that a node has*)
let rec degree : 'a gt -> int = 
  fun t ->
    match t with
    | Empty -> 0
    | Node(_, children) ->
      let max_degree = List.fold_left (fun curr child -> max curr (degree child)) 0 children in
      max (List.length children) max_degree