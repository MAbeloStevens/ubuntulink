
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1

   Name: Gabriel Castillo
   Pledge: I pledge my honor that I have abided by the Stevens honor system.
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

let a2 = {states = ["q0";"q1";"q2"];
          start = "q0";
          tf = [("q0",'a',"q1");("q0",'a',"q2")];
          final= ["q1"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



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

let apply_transition_function f st sym =
  match List.find_opt (fun (s, sy, _) -> s = st && sy = sym) f with
  | Some (_, _, next) -> Some next
  | None -> None

let next tf st sym =
  List.fold_left (fun out (s, sy, next) -> if s = st && sy = sym then next :: out else out) [] tf

let rec help fa cur input =
  match input with
  | [] -> List.mem cur fa.final
  | h::t -> 
      let next_states = next fa.tf cur h in
      if next_states = [] then false
      else List.exists (fun next_state -> help fa next_state t) next_states

let accept fa input =
  help fa fa.start input

let next tf st symbol =
  let filtered = List.filter (fun (s, sy, _) -> s = st && sy = symbol) tf in
  match filtered with
  | [] -> []
  | (_, _, next) :: _ -> [next]
  
let is_dup (s, sy, _) tf =
  List.exists (fun (_, sym, next) -> sym = sy && List.exists (fun (st, sym2, next2) -> st = s && sym2 = sy && next <> next2) tf) tf

let is_deterministic fa =
  not (List.exists (fun t -> is_dup t fa.tf) fa.tf)

let valid fa = 
  (is_deterministic fa) && (List.mem fa.start fa.states) && (List.exists (fun s -> List.mem s fa.states) fa.final)

let go fa cur out =
  let find = List.filter (fun (s, _, _) -> s = cur) fa.tf in
  match find with
  | [] -> out
  | (_, _, c)::t -> out @ [c]

let reachable fa =
  (go fa fa.start [fa.start]) @ fa.final

let non_empty fa =
  match fa.final with
  | [] -> false
  | h::t -> List.exists (fun state -> List.mem state (reachable fa)) fa.final
  
let remove_dead_states fa =
  let reachable_states = reachable fa in

  let rem_states = List.filter (fun s -> List.mem s reachable_states) fa.states in
  let rem_tf = List.filter (fun (s, _, f) -> List.mem s reachable_states && List.mem f reachable_states) fa.tf in
  let rem_final = List.filter (fun s -> List.mem s reachable_states) fa.final in
  
  {fa with states = rem_states; tf = rem_tf; final = rem_final}