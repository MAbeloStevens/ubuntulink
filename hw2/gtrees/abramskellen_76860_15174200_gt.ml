(*
General Tree Codes
Author: Kellen Abrams
*)

type 'a gt = Node of 'a *( 'a gt ) list


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
  | Node (x, []) -> 1
  | Node(x, lst) -> 1 + List.fold_left max 0 (List.map( fun tr -> height tr ) lst) 


let rec size : 'a gt -> int =
  fun t ->

  match t with 
  | Node (x, []) -> 1
  | Node(x, lst) -> 1 + List.fold_left (fun a b -> a+b) 0 (List.map( fun tr -> size tr ) lst) 


let rec paths_to_leaves : 'a gt -> 'b list  =
  fun t ->

  match t with
  | Node (x, []) -> [[]]
  | Node (x, lst) -> List.flatten (List.mapi (fun index tr -> List.map (fun p -> index :: p) (paths_to_leaves tr)) lst)
  

let is_leaf_perfect : 'a gt -> bool =
  fun t ->

  List.for_all(fun lst -> List.length lst == List.length (List.hd (paths_to_leaves t))) (paths_to_leaves t)


let rec preorder : 'a gt -> 'b list =
  fun t ->

  match t with 
  | Node (x, []) -> [x]
  |  Node (x, lst) -> List.fold_left (fun a b -> a @ b) [x] (List.map( fun tr -> preorder tr ) lst)


let rec mirror : 'a gt -> 'a gt =
  fun t ->

  match t with
  | Node (x, []) -> t
  | Node(x, lst) -> Node(x, List.rev (List.map( fun tr -> mirror tr) lst))


let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun f t ->

  match t with
  | Node (x, []) -> Node (f x, [])
  | Node (x, lst) -> Node(f x, (List.map( fun tr -> map f tr) lst))


let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
  fun f t ->

    match t with 
    | Node (x, lst) -> 
        let fd = List.map (fold f) lst
        in f x fd



let mirror' : 'a gt -> 'a gt = 
  fun t -> 

    fold (fun x lst -> Node (x, List.rev lst)) t
    

let rec get_all_degrees : 'a gt -> 'b list =
  fun t ->

  match t with 
  | Node (x, []) -> [0]
  | Node (x, lst) -> List.fold_left (fun a b -> a @ b) [List.length lst] (List.map( fun tr -> get_all_degrees tr ) lst)

let degree : 'a gt -> int = 
  fun t -> 
  List.fold_left max 0 (get_all_degrees t)