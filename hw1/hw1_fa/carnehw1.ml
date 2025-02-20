
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

let test_tf = [("q0",'a',"q1"); ("q0",'a',"q4"); ("q1",'b',"q1"); ("q1",'c',"q2")]

let a3 = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q0",'a',"q4"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a4 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
          final = ["q4"]
          }

let a5 = {states = ["q0";"q1";"q2";"q2"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
          final = ["q2"]}

(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let rec apply_transition_function (f : tf) (st : state) (sym : symbol) : state option =
  match f with
  | [] -> None
  | (a, b, next_state)::t ->
      if a = st && b = sym then 
        Some next_state
      else 
        apply_transition_function t st sym

let rec accept (fa : fa) (input : input) : bool =
  let rec read_input current_state input = 
    match input with 
    | [] -> List.mem current_state fa.final
    | sym::t ->
        match apply_transition_function fa.tf current_state sym with
        | Some next_state -> read_input next_state t
        | None -> false 
  in
  read_input fa.start input 
      
let rec next (f : tf) (st : state) (sym : symbol) : state list = 
  match f with
  | [] -> []
  | (a, b, next_state)::t ->
      if a = st && b = sym then 
        List.append [next_state] (next t st sym)
      else 
        next t st sym

let is_deterministic (fa : fa) : bool =
  let rec check transitions = 
    match transitions with
    | [] -> true
    | (st, sym, _)::t -> 
        if List.exists (fun (a, b, _) -> a = st && b = sym) t then
          false
        else 
          check t
  in
  check fa.tf

let valid (fa : fa) : bool =
  let rec check_final_states fstates states =
    match fstates with 
    | [] -> true
    | fstate::t -> 
      if List.mem fstate states then
        check_final_states t states
      else
        false
  in
  let rec check_duplicates states = 
    match states with 
  | [] -> true
  | h::t -> 
    if List.mem h t then
      false
    else
      check_duplicates t
  in
  is_deterministic fa 
  && List.mem fa.start fa.states
  && check_final_states fa.final fa.states
  && check_duplicates fa.states
  
let reachable (fa : fa) : state list =
  let rec explore f visited to_visit =
    match to_visit with
    | [] -> visited 
    | st::t ->
        if List.mem st visited then
          explore f visited t
        else
          let next_states =
            let rec find_next_states transitions reached =
              match transitions with
              | [] -> reached
              | (a, _, next)::t ->
                  if a = st then find_next_states t (List.append [next] reached)
                  else find_next_states t reached
            in
            find_next_states f []
          in
          explore f (List.append [st] visited) (List.append next_states t)
  in
  explore fa.tf [] [fa.start]
  
let non_empty (fa : fa) : bool =
  let rec check fstates states =
    match fstates with 
    | [] -> true
    | fstate::t -> 
      if List.mem fstate states then
        check t states
      else
        false
  in
  check fa.final (reachable fa)

let remove_dead_states (f : fa) : fa = 
  let rec remove_states states reachable_states =
    match states with
    | [] -> []
    | h::t ->
        if List.mem h reachable_states then 
          List.append [h] (remove_states t reachable_states)
        else 
          remove_states t reachable_states
  in
  let rec remove_transitions tf reachable_states =
    match tf with
    | [] -> []
    | (a, b, c)::t ->
      if List.mem a reachable_states then 
        List.append [(a, b, c)] (remove_transitions t reachable_states)
      else 
        remove_transitions t reachable_states
  in
  {states = remove_states f.states (reachable f);
  start = f.start;
  tf = remove_transitions f.tf (reachable f);
  final = remove_states f.final (reachable f)}