(* Payton Bates - I pledge my honor that I have abided by the Stevens Honor System *)

type program = int list;;

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1];;
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1];;

(* Maps 0 and 1 to itself, 2-3 to 4-5, and 4-5 to 2-3 *)
let mirror_image (list : int list) : int list =
  List.map (fun x ->
    match x with
    | x when x <= 1 -> x
    | x when x <= 3 -> x + 2
    | x when x <= 5 -> x - 2
    | _ -> x) list;;

(* Maps 0 and 1 to itself, 2-4 to 3-5, and 5 to 2 *)
let rotate_90_letter (list : int list) : int list =
  List.map (fun x ->
    match x with
    | x when x <= 1 -> x
    | x when x <= 4 -> x + 1
    | 5 -> 2
    | _ -> x) list;;

(* Maps the effects of rotate_90_letter to each element in the list *)
let rotate_90_word (list : int list list) : int list list =
  List.map rotate_90_letter list;;

(* Repeats x n number of times in a list *)
let rec repeat (n : int) (x : 'a) : 'a list =
  if n = 0 then []
  else [x] @ repeat(n-1) x;;

(* Enlarges p n-fold in a list using a map *)
let pantograph (n : int) (p : int list) : int list =
  List.flatten (
    List.map (fun x ->
      if x > 1 then repeat n x else [x]) p);;

(* Enlarges p n-fold using matching *)
let rec pantograph_nm (n : int) (p : int list) : int list =
  match p with
  | [] -> []
  | h::t -> 
      (if h > 1 then repeat n h else [h])
      @ pantograph_nm n t;;

(* Enlarges p n-fold using fold_right *)
let pantograph_f (n : int) (p : int list) : int list =
  List.fold_right (fun x a ->
    (if x > 1 then repeat n x else [x])
    @ a) p [];;

(* Returns a list of coordinates that the program visits given a starting coordinate and a list *)
let coverage ((a, b) : int*int) (list : int list) : (int*int) list =
  let rec helper ((x, y) : int*int) (lst : int list) =
    match lst with
    | [] -> [(x, y)]
    | h::t -> (x, y)::helper(
      match h with
      | 2 -> (x, y + 1)
      | 3 -> (x + 1, y)
      | 4 -> (x, y - 1)
      | 5 -> (x - 1, y)
      | _ -> (x, y)) t
  in helper (a, b) list;;

(* Returns a new list of tupled instructions and counts *)
let compress (list : int list) : (int*int) list =
  let rec helper (m : int) (n : int) (tail : int list) =
    match tail with
    | [] -> [(m,n)]
    | h::t ->
      if h = m then helper m (n + 1) t
      else (m, n)::helper h 1 t
  in match list with
  | [] -> []
  | h::t -> helper h 1 t;;

(* Uncompresses a compressed list using matching *)
let uncompress (list : (int*int) list) : int list =
  let rec helper (lst : (int*int) list) =
    match lst with
    | [] -> []
    | (m, n)::t -> (repeat n m) @ helper t
  in helper list;;

(* Uncompresses a compressed list using map *)
let uncompress_m (list : (int*int) list) : int list =
  List.flatten(
    List.map(fun (m, n) -> 
      repeat n m) list);;

(* Uncompresses a compressed list using fold *)
let uncompress_f (list : (int*int) list) : int list =
  List.fold_right (fun (m, n) t ->
    repeat n m @ t) list [];;

(* Eliminates redundant pen up/down instructions *)
(* Assumes the pen begins in the up position *)
let optimize (list : program) : program =
  let rec helper (list : program) (state : int) =
    match list with
    | [] -> []
    | h::t ->
      match h with
      | h when h <= 1 -> 
        if h = state
        then helper t state
        else h::helper t h
      | _ -> h::helper t state
  in helper list 1;;