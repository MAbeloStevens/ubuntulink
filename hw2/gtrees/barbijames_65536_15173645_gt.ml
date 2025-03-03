(*
General Trees
Author: James Barbi *)

type 'a gt = Node of 'a *('a gt) list;;

let t : int gt =
  Node (33,
    [Node (12, []);
    Node (77,
      [Node (37,
        [Node (14, [])]);
      Node (48, []);
      Node (103, [])])
  ])
;;

let rec height : 'a gt -> int =
  fun t ->
    match t with
    | Node(_, children) -> match children with
                            | [] -> 1
                            | _ -> 1 + List.fold_left (fun acc t -> max acc (height t)) 0 children
  ;;
;;

let rec size : 'a gt -> int =
  fun t ->
    match t with
    | Node(_, children) -> match children with
                            | [] -> 1
                            | _ -> 1 + List.fold_left (fun acc t -> acc + (size t)) 0 children
  ;;
;;

let rec paths_to_leaves : 'a gt -> int list list =
  fun t ->
    match t with
    | Node(_, children) -> match children with
                            | [] -> [[]]
                            | _ -> List.concat_map(fun (i, child) -> List.map (fun path -> i :: path) (paths_to_leaves child)) (List.mapi (fun i c -> (i, c)) children)
  ;;
;;

let rec is_leaf_perfect : 'a gt -> bool =
  fun t ->
    match t with
    | Node(_, children) -> match children with
                            | [] -> true
                            | _ -> let height_of_children = List.map height children in
                                  let all_same_height = List.for_all ((=) (List.hd height_of_children)) height_of_children in
                                  all_same_height && List.for_all is_leaf_perfect children
  ;;
;;

let rec preorder : 'a gt -> 'a list =
  fun t ->
    match t with
    | Node(value, children) -> value :: List.fold_right (fun child acc -> preorder child @ acc) children []
  ;;
;;

let rec reverse : 'a list -> 'a list =
  fun lst ->
    List.fold_left (fun acc x -> x :: acc) [] lst
  ;;
;;

let rec mirror : 'a gt -> 'a gt =
  fun t ->
    match t with
    | Node(value, children) -> Node(value, reverse (List.map mirror children))
  ;;
;;

let rec map : ('a -> 'b) -> 'a gt -> 'b gt =
  fun transformer t ->
    match t with
    | Node(value, children) -> Node(transformer value, (List.map (map transformer) children))
  ;;
;;

let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun transformer t ->
    match t with
    | Node(value, children) -> let child_results = List.map (fold transformer) children in
                                transformer value child_results
  ;;
;;

let mirror' : 'a gt -> 'a gt =
  fun t ->
    match t with
    | Node(value, children) -> fold (fun value children -> Node(value, reverse children)) t
  ;;
;;

let rec list_size lst =
  match lst with
  | [] -> 0
  | _ :: tail -> 1 + list_size tail
;;

let rec degree : 'a gt -> int =
  fun t ->
    match t with
    | Node(_, children) -> max (list_size children) (List.fold_left (fun acc t -> max acc (degree t)) 0 children)
  ;;
;;