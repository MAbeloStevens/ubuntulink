(*Quiz 1

Student Name 1: Sahil Virani
Student Name 2: Bhagawat Chapagain

Pledge 1: Sahil Virani       - I pledge my honor that I have abided by the Stevens Honor System
Pledge 2: Bhagawat Chapagain - I pledge my honor that I have abided by the Stevens Honor System
*)

let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]

let rec outgoing_nodes t n =
    match t with
    | [] -> []
    | h::tl ->
    if fst h=n
    then snd h :: outgoing_nodes tl n
    else outgoing_nodes tl n

let rec rem_dups : 'a list -> 'a list =
fun l ->
    match l with
    | [] -> []
    | h::t ->
        if List.mem h t
        then rem_dups t
        else h::rem_dups t

let rec nodes t = match t with
| [] -> []
| (x1,x2)::tl ->
let r = nodes tl
in (match List.mem x1 r, List.mem x2 r with
| true,true -> r
| false,true -> x1::r
| true,false -> x2::r
| _,_ -> x1::x2::r)

let rec leaves t =
    List.filter (fun n -> outgoing_nodes t n=[]) (nodes t)

let rec root t =
    let children = List.flatten @@ List.map (outgoing_nodes t) (nodes t)
    in List.filter (fun n -> not (List.mem n children)) (nodes t)

let rec is_binary t =
List.for_all ((>)3) @@ List.map List.length @@ List.map (outgoing_nodes t)(nodes t)

let rec subtree t (n:int) =
    let rec helper curr visited =
    match curr with
    | [] -> visited
    | h::tl ->
    let outgoing = outgoing_nodes t h
    in helper (tl @ outgoing) (List.map (fun m -> (h,m)) outgoing @ visited)
    in helper [n] []