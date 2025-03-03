(**************************************************************************
* Name: Gabriel Castillo
* Pledge: I pledge my honor that I have abided by the Stevens honor system.
***************************************************************************)

type dTree =
  | Leaf of int
  | Node of char*dTree*dTree

let tLeft = Node('w',Node('x',Leaf 2,Leaf 5),Leaf 8)
let tRight = Node('w',Node('x',Leaf 2,Leaf 5),Node('y',Leaf 7,Leaf 5))

let rec height t = 
  match t with
  | Leaf _ -> 1
  | Node(_,lt,rt) -> 1 + max (height lt) (height rt)

let rec size t =
  match t with
  | Leaf _ -> 1
  | Node(_,lt,rt) -> 1 + size lt + size rt

let rec paths t = 
  match t with
  | Leaf _ -> [[]]
  | Node(_,lt,rt) -> 
      let left = List.map (fun p -> 0 :: p) (paths lt) in
      let right = List.map (fun p -> 1 :: p) (paths rt) in
      left @ right

let is_perfect t = 
  let p = paths(t) in
  match p with
  | [] -> true
  | h::t -> let size = List.length h in List.for_all (fun s -> List.length s = size) t
    
let rec map f g t = 
  match t with
  | Leaf l -> Leaf (g l)
  | Node(l,lt,rt) -> Node(f l,map f g lt,map f g rt)

let rec list_to_tree t = 
  match t with
  | [] -> Leaf 0
  | h::tail -> Node(h, list_to_tree tail, list_to_tree tail)

let rec traverse t path f = 
  match t with
  | Leaf l ->
        (match List.find_opt (fun (p, _) -> p = path) f with
        | Some (_, n) -> Leaf n
        | None -> Leaf l)
  | Node(l, lt, rt) ->
      let left = traverse lt (path @ [0]) f in
      let right = traverse rt (path @ [1]) f in
      Node(l, left, right)

let replace_leaf_at t f =
  traverse t [] f

(*let rec bf_to_dTree l = 
  let l with
  | Empty -> Empty
  | (list, num) -> Node()*)