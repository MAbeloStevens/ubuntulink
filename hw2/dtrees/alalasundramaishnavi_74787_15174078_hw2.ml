(*Name: Aishnavi Alalasundram*)

type dTree =
| Leaf of int
| Node of char * dTree * dTree

let tLeft = 
  Node('y',
    Node('x', Leaf 2, Leaf 5),(*left subtree*)
    Leaf 8)(*right subtree*)

let tRight = 
  Node('y',
    Node('x', Leaf 2, Leaf 5),(*left subtree*)
    Node('y', Leaf 7, Leaf 5))(*right subtree*)

(*this returns the height of a tree*)
let rec height t = 
  match t with
  | Leaf _ -> 1 (* height of a leaf should be 1*)
  | Node(_, lt, rt) -> 1 + max(height lt) (height rt)

(*the size returns the number of inner nodes and leaves*)
let rec size t = 
  match t with
  | Leaf _ -> 1
  | Node(_, lt, rt) -> 1 + size lt + size rt

(*paths returns a list with all the paths to its leaves*)
let rec paths t =
   match t with
   | Leaf _ -> [[]] (*leaf returns empty list*)
   | Node(_, lt, rt) ->
       let left = List.map(fun x -> 0::x) (paths lt) in (*0 is added to the left subtrees paths*)
       let right = List.map(fun x -> 1::x) (paths rt) in (*1 is added to the right subtrees paths*)
       left @ right 

(*This determines whether a dTree is perfect, meaning if all leaves have the same depth*)
let rec is_perfect t = 
  let rec ds t d =
    match t with
    | Leaf _ -> [d] (*the list will have the depth in it*)
    | Node(_, lt, rt) -> (ds lt (d + 1)) @ (ds rt (d + 1)) (*all depths are put together*)
  in 
  let leaf_ds = ds t 0 in
  (*checks if the depths are the same*)
  let rec all_perfect l =
    match l with
    | [] -> true
    | h :: t ->
      let rec check_perfect gt = 
        match gt with
        | [] -> true
        | gh :: gt ->
          if gh = h
          then check_perfect gt
          else false
        in 
        check_perfect t
  in
  all_perfect leaf_ds (*confirms all depths are the same*)

(*This returns a new dTree resulting from t by applying f to the characters in each node
and g to the numbers in each leaf*)
let rec map f g t =
  match t with
  | Leaf n -> Leaf (g n)
  | Node(d,lt,rt) -> Node(f d, map f g lt, map f g rt)

(*This function forms the tree using the list *)
let rec list_to_tree l = 
  match l with 
  | [] -> Leaf 0
  | h :: t ->
    Node(h, list_to_tree t, list_to_tree t) (*each node is being created here and the left and right subtrees are being formed using recursion.*)

(*this function replaces all the leaves in t by the value indicated in the graph of the function*)
let rec replace_leaf_at t l =
  let rec replace t p v = 
    match (t, p) with
    | Leaf _ , [] -> Leaf v (*each leaf value is replaced with v if the path is empty*)
    | Node(d,lt,rt), 0 :: r -> Node(d, replace lt r v, rt) (*move left and replace leaf*)
    | Node(d,lt,rt), 1 :: r -> Node(d, lt, replace rt r v) (*move right and replace leaf*)
    | _ -> t
  in 
  match l with 
  | [] -> t
  | (p, v) :: r -> replace_leaf_at (replace t p v) r

(*This function takes a pair-encoding of a boolean function and returns its tree-encoding*)
let bf_to_dTree v g =
  let tree = list_to_tree v in (*list forms into tree*)
  replace_leaf_at tree g (*leaf is being replaced in path g*)