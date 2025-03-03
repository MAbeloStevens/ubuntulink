(* Author: Gustavo Balaguera *)
(* Due Date: February 23, 2025*)
(* Assignment: Homework 2 - G Trees *)

(* 
  Notes:
  - List.fold_left: This function takes a function, an initial accumulator value, and a list. It applies the function to the accumulator and the first element of the list, then to the result and the second element, and so on, returning the final accumulator value.
    Example: List.fold_left (fun acc x -> acc + x) 0 [1; 2; 3] = 6

  - List.mapi: This function takes a function and a list. It applies the function to each element of the list and its index, returning a list of the results.
    Example: List.mapi (fun i x -> (i, x)) [10; 20; 30] = [(0, 10); (1, 20); (2, 30)]

  - List.map: This function takes a function and a list. It applies the function to each element of the list, returning a list of the results.
    Example: List.map (fun x -> x * 2) [1; 2; 3] = [2; 4; 6]

  - List.concat: This function takes a list of lists and concatenates them into a single list.
    Example: List.concat [[1; 2]; [3; 4]; [5]] = [1; 2; 3; 4; 5]

  - List.for_all: This function takes a predicate function and a list. It returns true if the predicate returns true for all elements of the list, and false otherwise.
    Example: List.for_all (fun x -> x > 0) [1; 2; 3] = true
*)

type 'a gt = Node of 'a *('a gt ) list

(* Example G Tree *)
let t : int gt =
  Node (33 ,
  [ Node (12 ,[]) ;
  Node (77 ,
  [ Node (37 ,
  [ Node (14 , [])]) ;
  Node (48 , []) ;
  Node (103 , [])])
  ])

let mk_leaf : 'a -> 'a gt =
  fun n ->
  Node (n ,[])

(* Implemented functions: *)

let rec height t = 
  match t with
  | Node (_,[]) -> 1
  | Node (_,l) -> 1 + List.fold_left (fun a b -> max a b) 0 (List.map height l)

let rec size t =
  match t with 
  | Node(_,[]) -> 1
  | Node(_,l) -> 1 + List.fold_left (fun a b -> a + b) 0 (List.map size l)

let rec paths_to_leaves t =
  match t with
  | Node(_,[]) -> [[]]
  | Node(_,l) ->
      List.concat (List.mapi (fun i t' -> List.map (fun p -> i :: p) (paths_to_leaves t')) l)

let rec is_leaf_perfect t = 
  match t with
  | Node(_,[]) -> true
  | Node(_,l) -> 
      let first_size = size (List.hd l) in
      List.for_all (fun x -> size x = first_size) l && List.for_all is_leaf_perfect l

let rec preorder t =
  match t with
  | Node(d,l) -> d :: List.concat (List.map preorder l)

let rec mirror t =
  match t with
  | Node(d,l) -> Node(d, List.map mirror (List.rev l))

let rec map f t =
  match t with
  | Node(d,l) -> Node(f d, List.map (map f) l)

let rec fold f t =
  match t with
  | Node(d,l) -> f d (List.map (fold f) l)

let rec mirror' t = 
  fold (fun d l -> Node(d, List.rev l)) t

let rec degree t =
  match t with
  | Node(_,[]) -> 0
  | Node(_,l) -> max (List.length l) (List.fold_left (fun a b -> max a (degree b)) 0 l)