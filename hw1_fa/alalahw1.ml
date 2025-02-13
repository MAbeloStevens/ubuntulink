(*Name: Aishnavi Alalasundram*)
(*Pledge: I pledge my honor that I have abided by the Stevens Honor System.*)
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

(*Applies the transition function f to the symbol sym assuming that the current state is st. This should return the next state but if they dont find the next state it returns None.*)
let apply_transition_function (f : tf) (st : state) (sym : symbol)  = 
  match List.find_opt (fun (s, symbol, _) -> s = st && symbol = sym) f with 
  | Some (_, _, next_state) -> Some next_state
  | None -> None

(*Using recursion, this function will see if the string is accepted in finite automation.*)
let rec accept (fa : fa) (sym : input) : bool = 
  match sym with 
  | [] -> List.mem fa.start fa.final
  | sym :: t ->
    match apply_transition_function fa.tf fa.start sym with
    | Some next_state -> accept {fa with start = next_state} t
    | None -> false

(*This function returns the list of all the new states that are successors of some given state
with some given symbol.*)
let rec next (f : tf) (st : state) (sym : symbol) : state list = 
  match f with
  | [] -> []
  | (s, symbol, next_state) :: t ->
    if s = st && symbol = sym
    then next_state :: next t st sym
    else next t st sym

(*This checks if the automation is deterministic or not, returning true if the argument is a deterministic finite automaton and false if otherwise. A transition shouldn't have the same state and symbol.*)
let is_deterministic (fa : fa) : bool = 
  let rec check_dups trans =
    match trans with
    | [] -> true
    | (s, symbol, next_state) :: t ->
      let rec check_t t = 
        match t with
        | [] -> true
        | (s', symbol', next_state') :: t when s = s' && symbol = symbol' && next_state != next_state' -> false
        | _ :: t -> check_t t
      in 
      if not (check_t t)
      then false
      else check_dups t 
  in 
  check_dups fa.tf

(*This function makes sure the list of states has no duplicates.*)
let rec valid_states (states : state list) : bool = 
  match states with 
  | [] -> true
  | h :: t ->
    if List.mem h t 
    then false
    else valid_states t

(* This function checks if the start state valid meaning belongs to set of states.*)
let rec valid_start_state (start : state) (states : state list) : bool =
  match states with
  | [] -> false
  | h :: t -> 
    if h = start
    then true
    else valid_start_state start t

(* This function checks if the final states are valid in the state list.*)
let rec valid_final_state (final : state list) (states : state list) : bool =
  match final with
  | [] -> true
  | h :: t -> 
    if List.mem h states 
    then valid_final_state t states
    else false

(*This function checks the validity of FA.*)
let valid (fa : fa) : bool =
  let n = valid_states fa.states in
  let valid_start = valid_start_state fa.start fa.states in
  let valid_final = valid_final_state fa.final fa.states in
  let deterministic = is_deterministic fa in
  n && valid_start && valid_final && deterministic

let rec foldl a f l = 
  match l with
  | [] -> a
  | h::t -> foldl (f a h) f t

(*This function returns a list of states that are reachable from the start state*)
let rec reachable (fa : fa) : state list =
  let rec reach_ states = 
    let new_states = foldl states (fun next_state (s, _, next) ->
      if List.mem s states && not (List.mem next next_state)
      then next_state @ [next]
      else next_state) 
    fa.tf in 
    if new_states = states
      then states
    else reach_ new_states
  in 
  reach_ [fa.start]

(*This function determines whether a FA accepts at least one word or state.*)
let non_empty (fa : fa) : bool = 
  let reachable_states = reachable fa in
  let rec reach_final states =
    match states with 
    | [] -> false
    | state :: t ->
      if List.mem state fa.final 
      then true
      else reach_final t
  in
  reach_final reachable_states

(*This function removes all unreachable states from a valid FA.*)
let remove_dead_states (fa : fa) : fa =
  let reachable_states = reachable fa in
  let new_tf = List.filter (fun (src, _, tgt) ->
    List.mem src reachable_states && List.mem tgt reachable_states) fa.tf in
  let new_final = List.filter (fun s -> List.mem s reachable_states) fa.final in
  { states = reachable_states; start = fa.start; tf = new_tf; final = new_final }


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
