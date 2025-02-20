
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

let a3 = {states = ["q0";"q1";"q2";"q3";"q4"];
start = "q0";
tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q0",'a',"q0")
      ; ("q1",'c',"q2");  ("q3",'a',"q4")];
final= ["q3"]
}
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec mem e l =
  match l with
  | [] -> false
  | h::t -> (h=e) || mem e t

let rec rem_dups l =
  match l with
  | [] -> []
  | h::t ->
    if mem h t
    then rem_dups t
    else h:: rem_dups t

let rec determ_helper st sym res f = (*Returns true if there is an invalid transition for given transition*)
  match f with 
  | [] -> false
  | (tail_st, tail_sym, tail_res)::t -> if (tail_st = st && tail_sym = sym && not (tail_res = res)) then true else determ_helper st sym res t

let rec check_final_states f s = (* Returns true if every final state is in state list, false otherwise*)
  match f with 
  | [] -> true
  | h::t -> if not (mem h s) then false else check_final_states t s

let rec next_states transition_function start_state = 
  match transition_function with 
  | [] -> []
  | (st, sym, res)::t -> if st = start_state then start_state :: res :: next_states t start_state else next_states t start_state
  
let rec next_all_states transition_function start_state_list = (*just a map function for next_states IDK why I didnt make it a map but whatever*)
  match start_state_list with 
  | [] -> []
  | h::t -> next_states transition_function h :: next_all_states transition_function t

let rec together transition_function state_list = 
  let applied_tf = (rem_dups (List.concat (next_all_states transition_function state_list))) in (*Applies every valid transition function to every state in the reachable states list, and returns a list of all of the reachable states after that transition*)
  if (applied_tf = state_list) then applied_tf else together transition_function applied_tf (*If the result of applying every valid transition to every reachable state does not add anymore reachable states, then the function is finished.*)
  
(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
let rec apply_transition_function : tf -> state -> symbol -> state option = 
 fun f st sym ->
  match f with
  | [] -> None
  | (start_state, sy, end_state)::t ->
    if (start_state = st && sy = sym)
    then Some end_state
    else apply_transition_function t st sym

let rec accept : fa -> input -> bool = 
  fun f symbol_list  ->
  match symbol_list with 
  | [] -> List.mem f.start f.final
  | h::t ->  
    let post_transition = apply_transition_function f.tf f.start h in 
    match post_transition with
    | Some state -> accept {f with start = state} t
    | None -> false
    

let rec next : tf -> state -> symbol -> state list = 
  fun f st sym ->
    match f with
    | [] -> []
    | (start_state, sy, end_state)::t ->
      if (start_state = st && sy = sym)
      then end_state :: next t st sym
      else next t st sym


let rec is_deterministic : fa -> bool = 
  fun f ->
    match f.tf with
    | [] -> true
    | (st, sym, res)::t -> if (determ_helper st sym res t) then false else is_deterministic {f with tf = t}


let rec valid : fa -> bool = 
  fun f -> 
    if not (f.states = (rem_dups f.states)) then false (* If there are duplicate states then its not valid*)
    else if not (mem f.start f.states) then false (*If the start state isn't one of the states its not valid*)
    else if not (check_final_states f.final f.states) then false (*If every final state isn't one of the states then its not valid*)
    else is_deterministic f (*If the FA isn't deterministic then it's not valid*)


let rec reachable : fa -> state list =
  fun f ->
    together f.tf [f.start]


let non_empty : fa -> bool = 
  fun f ->
    let reachable_states = reachable f in
    List.length (List.filter (fun final_states -> mem final_states reachable_states) f.final) > 0 (*filters out all states from f.final that are not in reachable f, then returns whether or not the length of that list is > 0*)


let remove_dead_states : fa -> fa = 
  fun f -> 
    let reachable_states = reachable f in
    {{{f with states = reachable_states} (*replaces the states with only the reachable ones*)
    with tf = List.filter (fun (st, sym, res) -> (mem st reachable_states) && (mem res reachable_states)) f.tf} (*Takes each transition function and removes it if either the start or the tail is not reachable*)
    with final = List.filter (fun final_states -> mem final_states reachable_states) f.final} (*Takes each final state and removes it if it's not reachable*)