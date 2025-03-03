type 'a gt = Node of 'a * ('a gt) list;;
(*Sahana Ali 
Gtree 
CS 496 *)

(*map 
  *)
let rec map f t =
  match t with
  | Node (valu , children) -> Node (f valu , List.map (map f) children)


(* fold *)
let rec fold f (Node (value,  children)) =

  f value  (List.map (fold f)children)
let   sum t = fold  (fun   i rs -> i + List .  fold_left ( fun i j -> i + j ) 0 rs ) t;;
let mem t  e = fold(fun i rs -> i = e || List.exists  (fun i -> i ) rs) t;;
  

(* height *)
(*let rec height (Node (_, children) : 'a gt) : int =*)
let rec height t =
  match t with
  | Node (_, []) -> 1 
  | Node (_, children)-> 1+   List.fold_left(fun acc child -> max acc (height child))0  children 

(* size *)
let rec size t =
  match t with 

  | Node (_, []) -> 1 
  | Node (_, children) -> 1 +List.fold_left (fun acc child -> acc + size  child)0 children

(*path to leaves *)
let rec paths_to_leaves t =
  match t with
  | Node(_, []) ->[[]]  
  | Node (_, children) -> 


    let rec get_paths child_list index =
      match child_list with
      | [] -> []  

      | child :: rest -> 
          let child_paths = paths_to_leaves child  in
          let updated_paths = List.map(fun path -> index :: path)child_paths   in
          updated_paths @(get_paths rest (  index + 1)   )
    in
    get_paths children 0

(* leaf_perfect*)
let rec is_leaf_perfect t =


  let rec find_leaf_depths t depth =
    match t with
    | Node(_, [])->   [depth] 
    | Node(_, children)   ->List.flatten (List.map(fun    child -> find_leaf_depths  child (depth + 1)) children)
  in
  (*helper fun to find leaf depth*)
  let leaf_depths = find_leaf_depths t 0 in


  match leaf_depths with
  | [] -> true 
  | h :: t -> List.for_all (fun d -> d = h) t  

(* preorder*)
let rec preorder t =

  match t with
  | Node(value, children) ->
    let rec traverse lst =

      match lst with
      | [] ->[]
      | h :: t -> preorder h @ traverse t
    in

    value :: traverse children

(* mirror*)
let rec mirror t =


  match t with
  | Node (value, children) ->

    let rec mirror_children lst acc =
      match lst with
      | [] -> acc
      | hd :: tl -> mirror_children tl  ((mirror hd) :: acc)
    in
    Node (value, mirror_children children [])
 (*mirror fold *)
 let mirror' t =
  fold (fun value children -> Node (value, List.rev children)) t

(* Degree*)
let rec degree t =

  match t with
  | Node (_, children) ->
    List.fold_left(fun acc child -> max acc (degree child)) (List.length children) children


(*sample testing*)
let t : int gt =
  Node (33 , [Node (12 ,[]) ; Node (77 ,[Node (37 , [ Node (14 , [])]) ;Node (48 , []);Node (103 , [])])
])

