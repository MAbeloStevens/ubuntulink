
(*Name: Lucas Burdian
  Pledge: "I pledge my honor that I have abided by the Stevens honor system."
  Date: 2/24/2025*)
(* General Tree Definition *)
type 'a gt = Node of 'a * ('a gt) list

(* 1. Compute the height of a general tree *)
let rec height (Node (_, children)) =
  match children with
  | [] -> 1
  | _ -> 1 + List.fold_left (fun acc child -> max acc (height child)) 0 children

(* 2. Compute the size of a general tree *)
let rec size (Node (_, children)) =
  1 + List.fold_left (fun acc child -> acc + size child) 0 children

(* 3. Compute all paths to leaves *)
let rec paths_to_leaves (Node (_, children)) =
  match children with
  | [] -> [[]]
  | _ -> 
    List.mapi 
      (fun i child -> List.map (fun path -> i :: path) (paths_to_leaves child)) 
      children
    |> List.flatten

(* 4. Check if a tree is leaf-perfect *)
let is_leaf_perfect tree =
  let rec depths (Node (_, children)) depth =
    match children with
    | [] -> [depth]
    | _ -> List.flatten (List.map (fun child -> depths child (depth + 1)) children)
  in
  let all_depths = depths tree 0 in
  List.for_all ((=) (List.hd all_depths)) all_depths

(* 5. Preorder traversal *)
let rec preorder (Node (value, children)) =
  value :: List.flatten (List.map preorder children)

(* 6. Mirror a tree *)
let rec mirror (Node (value, children)) =
  Node (value, List.rev (List.map mirror children))

(* 7. Apply a function to every element in a tree *)
let rec map f (Node (value, children)) =
  Node (f value, List.map (map f) children)

(* 8. Fold over a tree *)
let rec fold f (Node (value, children)) =
  f value (List.map (fold f) children)

(* Example usage of fold *)
let sum t = fold (fun i rs -> i + List.fold_left (+) 0 rs) t
let mem t e = fold (fun i rs -> i = e || List.exists (fun x -> x) rs) t

(* 9. Mirror using fold *)
let mirror' tree =
  fold (fun v children -> Node (v, List.rev children)) tree

(* 10. Compute the degree of a tree *)
let rec degree (Node (_, children)) =
  max (List.length children) (List.fold_left (fun acc child -> max acc (degree child)) 0 children)


(* Test Cases *)
let t = Node (33, [
  Node (12, []);
  Node (77, [
    Node (37, [Node (14, [])]);
    Node (48, []);
    Node (103, [])
  ])
])

let () =
  assert (height t = 4);
  assert (size t = 7);
  assert (paths_to_leaves t = [[0]; [1; 0; 0]; [1; 1]; [1; 2]]);
  assert (is_leaf_perfect t = false);
  assert (preorder t = [33; 12; 77; 37; 14; 48; 103]);
  assert (mirror t = Node (33, [
    Node (77, [
      Node (103, []);
      Node (48, []);
      Node (37, [Node (14, [])])
    ]);
    Node (12, [])
  ]));
  assert (map (fun i -> i > 20) t = Node (true, [
    Node (false, []);
    Node (true, [
      Node (true, [Node (false, [])]);
      Node (true, []);
      Node (true, [])
    ])
  ]));
  assert (sum t = 324);
  assert (mem t 12 = true);
  assert (mem t 33 = true);
  assert (mem t 35 = false);
  assert (mirror' t = mirror t);
  assert (degree t = 3);
  print_endline "All tests passed!"
            
                      