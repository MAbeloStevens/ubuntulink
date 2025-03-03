type 'a gt = Node of 'a * ('a gt) list

let t : int gt =
 Node (33,
	[Node (12,[]) ;
	 Node (77,
		[Node (37,
			[Node (14, [])]) ;
		Node (48, []) ;
		Node (103, [])])
	])



let rec maxl l =
  match l with
  | [] -> failwith "maxl: empty list"
  | [x] -> x
  | h::t -> max h (maxl t)


(*1*)
let rec height t =
	match t with
 	| Node(_, []) -> 1 
	| Node(_, children) -> 1 + maxl (List.map height children)


(*2*)
let rec suml l =
	match l with
	| [] -> 0
	| h::t -> h + suml t

let rec size (Node(d,ch)) =
	1 + suml (List.map size ch)


(*3*)
let rec paths_to_leaves t =
	let rec paths_to_leaves_helper children pos =
    		match children with
    		| [] -> []
    		| h::t ->
        		let paths = List.map (fun path -> pos::path) (paths_to_leaves h) in
        		paths @ paths_to_leaves_helper t (pos + 1)
  		in
  		match t with
  		| Node(_, []) -> [[]]  
  		| Node(_, children) -> paths_to_leaves_helper children 0


(*4*)
let is_leaf_perfect t =
	let rec is_leaf_perfect_helper depth (Node(_, children)) =
    		match children with
    		| [] -> [depth]
    		| _ -> List.concat (List.map (is_leaf_perfect_helper (depth + 1)) children)
  	in
  	let depths = is_leaf_perfect_helper 0 t in
	let rec are_items_equal = function
  		| [] | [_] -> true
  		| head::next::rest -> head = next && are_items_equal (next::rest)
  	in
  	are_items_equal depths



(*5*)
let rec preorder t =
	match t with
	| Node(d, children) -> [d] @ List.flatten (List.map preorder children)

(*6*)
let rec mirror t =
	match t with
	| Node(d, children) -> Node(d, List.rev (List.map mirror children))

(*7*)
let rec map f t =
 	match t with
	| Node(d, children) -> Node(f d, List.map (map f) children)


(*8*)
let rec fold f (Node(d, children)) =
  f d (List.map (fold f) children)


let sum t =
	fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t
let mem t e =
	fold (fun i rs -> i = e || List.exists (fun i -> i) rs) t


(*9*)
let mirror' t = 
  fold (fun d rs -> Node(d, List.rev rs)) t



(*10*)
let rec degree t =
	match t with
  	| Node(_, children) ->
    	let child_degrees = List.map (fun (Node(_, ch)) -> 
	List.length ch) children
    	in max 0 (maxl child_degrees)
