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

let pt : int gt =
    Node (33 ,
      [ Node (12 ,[
        Node (48 , [])
      ]) ;
      Node (77 ,
        [ Node (37 ,[])])
    ])



let mk_leaf : 'a -> 'a gt =
  fun n ->
  Node (n ,[])

let rec height : 'a gt -> int =
  fun t -> 
  match t with
  | Node (_, []) -> 1
  | Node(_, children) -> 1 + (List.fold_left max min_int (List.map (height) children) )


  let size : 'a gt -> int =
    fun t -> 
    match t with
    | Node (_, []) -> 1
    | Node(_, children) -> 1 + (List.fold_left (+) 0 (List.map (size) children) )

let paths_to_leaves : 'a gt -> int list list =
    fun t -> 
    match t with
    | Node (data, []) -> [[]]
    | Node(data, children) -> paths_helper t []
      
      
     (* range ((List.length children) - 1) 
     List.mapi (fun i a -> [i] @ a) (List.flatten (List.map paths_to_leaves children))  *)
  let rec paths_helper t ancestry = 
    match t with
    | Node (data, []) -> [ancestry]
    | Node(data, children) -> 
    List.flatten (List.mapi (fun i c -> paths_helper c (ancestry @ [i])) children )
  
(* let rec range : int -> int list = 
  fun i ->
    if (i = 0) then [0] else [0] @ (List.map (fun a -> a+1) (range (i-1) )) *)

let rec is_leaf_perfect : 'a gt -> bool = 
  fun t -> 
    match t with
    | Node (data, []) -> true
    | Node(data, children) -> 
      List.fold_left (=) true (List.map (fun a -> a = height (List.nth children 0) ) (List.map height children)) 
      && 
      List.fold_left (=) true (List.map (fun a -> is_leaf_perfect a) children)

let rec preorder : 'a gt -> 'a list =
  fun t -> 
    match t with
    | Node (data, []) -> [data]
    | Node(data, children) -> [data] @ (List.flatten (List.map (fun a -> preorder a) children))

let rec mirror : 'a gt -> 'a gt =
  fun t -> 
    match t with
    | Node (data, []) -> Node (data, [])
    | Node(data, children) -> Node(data, List.map mirror (List.rev children) )

let rec map : ('a -> 'b) -> 'a gt -> 'b gt = 
  fun f t -> 
    match t with
    | Node (data, []) -> Node (f data, [])
    | Node(data, children) -> Node(f data, List.map (fun a -> map f a) children)

(*let rec fold_helper : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
  fun knit t -> 
    match t with
    | Node (data, []) -> knit data []
    | Node(data, children) -> knit data (List.map (fun a -> fold_helper knit a) children) *)
    
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
  fun knit t -> 
    match t with
    | Node (data, []) -> knit data []
    | Node(data, children) -> knit data (List.map (fun a -> fold knit a) children)

    let sum t =
      fold (fun i rs -> i + List.fold_left (fun i j -> i + j ) 0 rs) t
    let mem : 'a gt -> 'a -> bool =
      fun t e ->
      fold (fun i rs -> (i=e) || List.exists (fun a -> a) rs) t

let mirror' : 'a gt -> 'a gt =
fun t -> 
  match t with
  | Node(data, children) ->  fold (fun i rs -> Node(i, List.rev children) ) t

let rec degree : 'a gt -> int = 
  fun t -> 
    match t with
    | Node (data, []) -> 0
    | Node(data, children) -> max (List.length children) (List.fold_left max min_int (List.map (degree) children))
