(*
  Author: Jaran Binning
  gt.ml
*)

type 'a gt = Node of 'a *('a gt) list

let t : int gt =
  Node (33 ,
    [Node (12,[]);
     Node (77,
          [Node (37 ,
                [Node (14 , [])]) ;
           Node (48 , []) ;
           Node (103 , [])])
        ])

(*
        33
      /    \
    12      77
          / |  \
        37  48  103
        /
      14
*)

let mk_leaf : 'a -> 'a gt =
  fun n ->
    Node (n,[])
  
let rec height : 'a gt -> int =
  fun t ->
    match t with
    | Node (_,[]) -> 1
    | Node (_,t) -> 1 + List.fold_left (fun f x -> max f (height x)) 0 t

let rec size : 'a gt -> int =
  fun t ->
    match t with
    | Node (_,[]) -> 1
    | Node (_,t) -> 1 + List.fold_left (fun f x -> f + size x) 0 t

(*FIX ME*)

let path_to_leaves t =
  let rec helper path t =
    match t with
    | Node (_, []) -> [List.rev path]
    | Node (_, ts) ->
      List.concat (List.mapi (fun i t -> helper (i :: path) t) ts)
  in
  helper [] t


let is_leaf_perfect t =
  let rec helper depth t =
    match t with
    | Node (_, []) -> [depth]
    | Node (_, ts) -> List.concat (List.map (helper (depth + 1)) ts)
  in
  let leaf_depths = helper 0 t in
  match leaf_depths with
  | [] -> true
  | hd :: tl -> List.for_all ((=) hd) tl

let rec preorder : 'a gt -> 'a list =
  fun t ->
    match t with
    | Node (n,[]) -> [n]
    | Node (n,t) -> n :: List.flatten (List.map preorder t)


let rec mirror : 'a gt -> 'a gt =
  fun t ->
    match t with
    | Node (n,[]) -> Node (n,[])
    | Node (n,t) -> Node (n,List.map mirror (List.rev t))

let rec map f t =
  match t with
  | Node (n,[]) -> Node (f n,[])
  | Node (n,t) -> Node (f n,List.map (map f) t)


let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f t ->
    match t with
    | Node (n,[]) -> f n []
    | Node (n,t) -> f n (List.map (fold f) t)


let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i + j) 0 rs) t

let mem t e =
  fold (fun i rs -> i = e || List.exists (fun i -> i ) rs) t

let mirror' t =
  fold (fun i rs -> Node (i, List.rev rs)) t

let degree t =
  fold (fun _ rs -> max (List.length rs) (List.fold_left max 0 rs)) t