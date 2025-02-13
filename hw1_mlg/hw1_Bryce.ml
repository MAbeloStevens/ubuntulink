(*
Name: William Bryce
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type program = int list

let mirror_helper l = 
    match l with
    | 2 -> 4
    | 3 -> 5
    | 4 -> 2
    | 5 -> 3
    | l -> l

let mirror_image : int list -> int list =
    fun x -> List.map mirror_helper x

let rotate_90_letter_helper l =
    match l with
    | 0 -> 0
    | 1 -> 1
    | 5 -> 2
    | l -> l+1

let rotate_90_letter : int list -> int list = 
    fun x -> List.map rotate_90_letter_helper x

let rotate_90_word : int list list -> int list list =
    fun x -> List.map rotate_90_letter x

let rec repeat : int -> 'a -> 'a list =
    fun n x ->
    match n with
    | 0 -> []
    | n -> x :: repeat (n-1) x

let pantograph_helper n e =
    match e with
    | 0 -> [0]
    | 1 -> [1]
    | e -> repeat n e

let pantograph : int -> int list -> int list =
    fun n p -> List.flatten (List.map (fun e -> (pantograph_helper n e)) p)

let rec pantograph_nm : int -> int list -> int list =
    fun n p ->
    match p with
    | [] -> []
    | x::xs ->
        if x=0 || x=1
        then x :: (pantograph_nm n xs)
        else (repeat n x) @ (pantograph_nm n xs)

let pantograph_f : int -> int list -> int list =
    fun n p -> List.fold_left (fun acc e -> acc @ (pantograph_helper n e)) [] p

let coverage_helper (x,y) l =
    match l with
    | 2 -> (x,y+1)
    | 3 -> (x+1,y)
    | 4 -> (x,y-1)
    | 5 -> (x-1,y)
    | l -> (x,y)

let rec coverage : int*int -> int list -> (int*int) list =
    fun (x,y) l ->
    match l with
    | [] -> [(x,y)]
    | h::t -> (x,y) :: (coverage (coverage_helper (x,y) h) t)

let rec compress_helper (x,y) h =
    match h with
    | [] -> [(x,y)]
    | h::t ->
        if h = (fst (x,y))
        then compress_helper (x,y+1) t
        else [(x,y)] @ (compress_helper (h,1) t)

let compress : int list -> (int*int) list =
    fun x ->
    match x with
    | [] -> []
    | x:: xs -> compress_helper (x,1) xs

let rec uncompress : (int*int) list -> int list =
    fun h ->
    match h with
    | [] -> []
    | h::t -> (repeat (snd h) (fst h)) @ (uncompress t)

let uncompress_m : (int*int) list -> int list =
    fun l -> List.flatten (List.map (fun x -> (repeat (snd x) (fst x))) l)

let uncompress_f : (int*int) list -> int list =
    fun l -> List.fold_left (fun acc x -> acc @ (repeat (snd x) (fst x))) [] l

let rec optimize_helper c h =
    match h with
    | [] -> []
    | h::t ->
        if (h=c)
        then optimize_helper c t
        else if (h=0 || h=1) 
        then h :: optimize_helper h t
        else h :: optimize_helper c t

let optimize : program -> program =
    fun x -> optimize_helper 1 x