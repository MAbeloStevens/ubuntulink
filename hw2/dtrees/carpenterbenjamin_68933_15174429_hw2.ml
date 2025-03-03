type dtree = 
| Leaf of int
| Node of char*dtree*dtree

let tLeft : dtree = 
  Node('w', 
    Node('x', 
      Leaf(2), 
      Leaf(5)
    ), 
    Leaf(8)
  )

let tRight : dtree = 
  Node('w',
    Node('x',
      Leaf(2),
      Leaf(5)
    ),
    Node('y',
      Leaf(7),
      Leaf(5)
    )
  )
  let tMiddle : dtree = 
    Node('w',
      Node('x',
        Leaf(2),
        Leaf(5)
      ),
      Node('y',
        Node('z',
          Leaf(9),
          Leaf(10)
        ),
        Leaf(6)
      )
    )
  

let rec height t =
  match t with
  | Leaf(x) -> 1
  | Node(x, y, z) -> 1 + max (height y) (height z) 

let rec size t = 
  match t with
  | Leaf(x) -> 1
  | Node(x, y, z) -> 1 + (size y) + (size z)

let rec paths t =
  match t with
  | Leaf(x) -> [[]]  
  | Node(x, left, right) ->
      let left_paths = List.map (fun x -> 0 :: x) (paths left) in
      let right_paths = List.map (fun x -> 1 :: x) (paths right) in
      left_paths @ right_paths
  

let isPerfect t = 
  let treeHeight = (height t) in
  let treeSize = size t in
  ((Int.shift_left 1 treeHeight) - 1) = treeSize


let rec map f g t = 
  match t with
  Leaf(x) -> Leaf(g x)
  | Node(x, y, z) -> Node(f x, map f g y, map f g z)

let rec list_to_tree : char list -> dtree =
  fun l ->
    match l with 
    | [] -> Leaf(0)
    | h::t -> Node(h, list_to_tree t, list_to_tree t)



let rec wapdeedoo t path res = 
  match path with 
  | [] -> Leaf(res)
  | h::tail -> 
    match t with 
    | Node(x,y,z) -> if(h = 0) then Node(x, wapdeedoo y tail res, z) else Node(x,y,wapdeedoo z tail res)
    | Leaf(x) -> raise (Invalid_argument "path")


let rec replace_leaf_at : dtree -> (int list * int) list -> dtree =
  fun t f ->
    match f with
    | [] -> t
    | (path, res)::tail -> replace_leaf_at (wapdeedoo t path res) tail


let rec bf_to_dtree : char list * (int list * int) list -> dtree = 
  fun (charList, encoding) ->
    replace_leaf_at (list_to_tree charList) encoding