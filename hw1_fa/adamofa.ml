(* Avery Adamo *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* This code was presented in class to check for duplicates in a list *)
let rec rem_dups l =
  match l with
  | [] -> []
  | h::t ->
    if List.mem h t
    then rem_dups t
    else h :: rem_dups t

(* Returns true if there are duplicates *)
let rec rem_dups' l =
  match l with
  | [] -> false
  | h::t ->
    if List.mem h t
    then true
    else rem_dups' t

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let rec apply_transition_function : tf -> state -> symbol -> state option =
  fun f st sym ->
  match f with
  | [] -> None
  | (current, symbol, next)::t ->
    if current=st && symbol=sym
    then Some next
    else apply_transition_function t st sym


let rec accept : fa -> input -> bool =
  fun a input ->
  let rec accept_helper : tf -> input -> state -> state =
    fun f input state ->
    match input with
    | [] -> state
    | h::t ->
      match apply_transition_function f state h with
      | Some next_state -> accept_helper f t next_state
      | None -> ""
    in
  match accept_helper a.tf input a.start with
  | "" -> false
  | state -> List.mem state a.final


let rec next : tf -> state -> symbol -> state list =
  fun f st sym ->
  match f with
  | [] -> []
  | (current, symbol, succ)::t ->
    if current=st && symbol=sym
    then succ :: next t st sym
    else next t st sym 


let is_deterministic : fa -> bool =
  fun a ->
  let rec is_deterministic_helper : tf -> bool =
    fun f ->
    match f with 
    | [] -> true
    | (current, symbol, succ)::t ->
      if List.length (next f current symbol) > 1 
      then false
      else is_deterministic_helper t
    in
  is_deterministic_helper a.tf
  

let valid : fa -> bool =
  fun a ->
  let state_dups = rem_dups' a.states in
  let valid_start = List.mem a.start a.states in
  let valid_final = List.for_all (fun s -> List.mem s a.states) a.final in
  let deterministic = is_deterministic a in

  (state_dups = false) && valid_start && valid_final && deterministic


let reachable : fa -> state list =
  fun a ->
  let rec reachable_helper : fa -> state list -> state list -> state list =
  fun a to_visit visited ->
  match to_visit with
  | [] -> visited
  | h::t ->
    (* Gathers all transitions which begin with state h *)
    let start_states = List.filter(fun (x, y, z) -> x=h) a.tf in 
    (* Gets the list of successors of h with no duplicates*)
    let succ_list = if (List.mem h a.final) then [] else rem_dups (List.flatten (List.map (fun (x, y, z) -> next a.tf x y) start_states)) in

    (* Adds h to visited if it isn't already there *)
    let new_v = if not (List.mem h visited) then h :: visited else visited in
    
    (* Adds new states to to_visit if they haven't already been visited *)
    let new_tv = (List.filter (fun s -> not (List.mem s visited)) succ_list) @ t in

    reachable_helper a new_tv new_v in
  let states_to_visit = [a.start] in
  reachable_helper a states_to_visit []


let non_empty : fa -> bool =
  fun a ->
  let reachable_states = reachable a in
  List.exists (fun s -> List.mem s a.final) reachable_states


let rec remove_dead_states : fa -> fa =
  fun a ->
  let reachable_states = reachable a in
  (* Find dead states by filtering reachable states out of state list *)
  let dead_states = List.filter (fun s -> not (List.mem s reachable_states)) a.states in
  
  (* Filter dead states out of fa components *)
  let new_states = List.filter (fun s -> not (List.mem s dead_states)) a.states in
  let new_tf = List.filter (fun (x, y, z) -> not (List.mem x dead_states || List.mem z dead_states)) a.tf in
  let new_final = List.filter (fun s -> not (List.mem s dead_states)) a.final in

  {states = new_states;
  start = a.start;
  final = new_final;
  tf = new_tf;}
