(*  Name: Aidan Brown
    Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a gt = Node of 'a *( 'a gt ) list

let t1 : int gt =
  Node(33, [Node(1, []) ; Node(2, []) ; Node(3, []) ; Node(4, []) ; Node(5, [])])

let t : int gt =
  Node (33 ,
    [ Node (12 ,[]) ;
      Node (77 ,
        [ Node (37 ,
            [ Node (14 , [])]) ;
          Node (48 , []) ;
        Node (103 , [])])
    ])

let rec height : 'a gt -> int =
  fun t ->
  match t with
  | Node(_, []) -> 1
  | Node(_, st) -> 
    let h_help a c = max a (height c) in
    1 + List.fold_left h_help 0 st

let rec size : 'a gt -> int =
  fun t ->
  match t with
  | Node(_, []) -> 1
  | Node(_, st) -> 
    let s_help a c = a + size c in
    1 + List.fold_left s_help 0 st

let rec paths_to_leaves : 'a gt -> int list list =
  fun t ->
  let p_help i st = List.map (fun l -> i::l) (paths_to_leaves st) in
  match t with
  | Node (_, []) -> [[]]
  | Node (_, st) -> List.flatten (List.mapi p_help st)
    

let is_leaf_perfect : 'a gt -> bool =
  fun t ->
  let rec rm_dup l =
    match l with
    | [] -> []
    | h::t when List.mem h t -> rm_dup t
    | h::t -> h:: rm_dup t
  in
  if List.length (rm_dup (List.map List.length (paths_to_leaves t))) > 1 
  then false
  else true

let rec preorder : 'a gt -> 'a list = 
  fun t ->
  match t with
  | Node(v, st) -> v :: List.flatten (List.map preorder st)

let rec mirror : 'a gt -> 'a gt = 
  fun t ->
  match t with
  | Node(_, []) -> t
  | Node(v, st) -> Node(v, (List.rev_map mirror st))

let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun f t ->
  match t with
  | Node(v, []) -> Node((f v), [])
  | Node(v, st) -> 
    let map_help t = map f t in
    Node((f v), (List.map map_help st))

let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f t ->
  match t with
  | Node(v, []) -> f v []
  | Node(v, st) ->
    let fold_help t = fold f t in
    f v (List.map fold_help st)

let mirror' : 'a gt -> 'a gt =
  fun t ->
  fold (fun i rs -> Node (i, List.rev rs)) t

let rec degree : 'a gt -> int =
  fun t ->
  match t with 
  | Node(v, st) -> List.fold_right max (List.map degree st) (List.length st)