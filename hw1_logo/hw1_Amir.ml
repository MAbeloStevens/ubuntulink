(*Harishan Amirthanathan
I pledge my honor that I have abided by the Stevens Honor System
*)

type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [ 0 ; 2 ; 2 ; 3 ; 3 ; 5 ; 5 ; 4 ; 3 ; 5 ; 4 ; 3 ; 3 ; 5 ; 5 ; 1 ]

let rec map : ('a -> 'b) -> 'a list -> 'b list =
    fun f l ->
    match l with
    | [] -> []
    | h::t -> f h :: map f t  

(* 1: Returns mirror image by swapping directions *)
let mirror_image (n:int list ) : int list =
    List.map (fun x ->
        match x with
        | 2 -> 4
        | 4 -> 2
        | 3 -> 5
        | 5 -> 3
        | x -> x) n

(*2: Rotates a program 90 degrees by remapping directions *)
let rotate_90_letter (n:int list ) : int list =
    List.map (fun x ->
        match x with
        | 2 -> 3
        | 3 -> 4
        | 4 -> 5
        | 5 -> 2
        | x -> x) n

(*3: Rotates a word 90 degress *)
let rotate_90_word (n:int list list) : int list list =
    List.map rotate_90_letter n

(*4: Returns a list with n copies of element e *)
let rec repeat (n: int) (e: 'a) : 'a list =
    match n with
    | 0 -> []
    | m -> e :: repeat (m-1) e

(*5: Enlarges a program using map: repeats drawing instructions n times *)
let pantograph (n: int) (e: int list) : int list =
    List.concat(List.map(fun x ->
        if x > 1 then repeat n x else [x]
    ) e) 

(* Enlarges a program recursively without using map *)
let rec pantograph_nm (n: int) (e: int list) : int list =
    match e with
    | [] -> []
    | h :: t -> 
        if h > 1 then List.concat[repeat n h; pantograph_nm n t]
        else h :: pantograph_nm n t

(* Enlarges a program using fold_right *)
let pantograph_f (n: int) (e: int list) : int list =
    List.fold_right (fun x acc -> 
        if x > 1 then List.concat[repeat n x; acc]
        else List.concat[[x];acc])
         e []

let helper1 (x,y) n =
    match n with
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1,y)
    | _ -> (x,y)

(*6: Returns the list of coordinates visited by a program *)
let coverage ((x,y): int*int) (n: int list) : (int*int) list =
    let rec helper2 (x,y) n e = 
        match n with
        | [] -> List.rev e 
        | h :: t -> helper2 (helper1 (x, y) h) t ((helper1 (x,y) h) :: e)
    in helper2 (x,y) n [(x,y)]

(*7: Compresses a program by grouping adjacent identical instructions *)    
let compress (n: int list) : (int*int) list =
    List.rev (List.fold_left (fun acc x ->
        match acc with
        | (prev,e) :: rest when prev = x -> (prev,e+1) :: rest
        | _ -> (x,1)::acc) [] n)

(*8: Reconstructs a program from its compressed form (recursive) *)
let rec uncompress (n: (int * int) list) : int list =
    match n with
    | [] -> []
    | (x,y) :: t -> repeat y x @ uncompress t

(* Uncompresses using map *)
let uncompress_m (n: (int * int) list) : int list =

    List.concat (List.map(fun (x,y) -> repeat y x) n)
(* Uncompresses using fold_right *)
let uncompress_f (n: (int * int) list) : int list =
    List.fold_right (fun (x,y) acc -> repeat y x @ acc) n []

(*9: Removes redundant pen commands *)
let optimize (n: int list) : int list =
  let rec x y lst acc = 
    match lst with
    | [] -> List.rev acc
    | h :: t when (h = 0 || h = 1) && h = y -> x y t acc
    | h :: t -> x h t (h :: acc)
  in x 1 n []
