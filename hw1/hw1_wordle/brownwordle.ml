(*
WORDLE GAME
Author: 
*)
(* Install opam package "csv" with "opam install csv" *)
#require "csv"


type game_state = { w_h : string list;
                    kb : string list;
                    the_word: string }

(* initial state *)
let state =
  { w_h = ["_____";"_____";"_____";"_____";"_____";"_____"];
    kb = ["QWERTYUIOP";" ASDFGHJKL";"  ZXCVBNM"];
    the_word = "OCAML"
  }

  let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
    while true; do
    lines := [String.trim @@ input_line chan] :: !lines
    done; !lines
    with End_of_file ->
    close_in chan;
    List.rev !lines

  let word_bank_src = read_file "word_bank.csv"



let rec process_data_help src =
  match src with
  | [] -> []
  | h::t -> h @ process_data_help t 

let rec process_data src =
	(* TODO *)
  List.map String.uppercase_ascii (process_data_help src) 


let process_data_fold src =
	(* TODO *)
	List.map String.uppercase_ascii (List.fold_right (@) src [])

let word_bank = process_data word_bank_src

let is_word_valid w = List.mem w word_bank

let rec explode s i a = 
  if i < String.length s 
  then explode s (i + 1) (a @ [Char.escaped s.[i]])
  else a

let rec space_out_help l = 
  match l with
  | [] -> ""
  | [x] -> x ^ space_out_help []
  | h::t -> h ^ " " ^ space_out_help t 

let space_out_word s =
  space_out_help (explode s 0 [])

let rec rm_from_row row letter =
  match row with
  | [] -> ""
  | h::t ->
    if h = (Char.escaped letter)
    then " " ^ rm_from_row t letter
    else h ^ rm_from_row t letter

let rec rm_letter kb letter =
	match kb with
  | [] -> []
  | h::t ->
    if List.mem (Char.escaped letter) (explode h 0 [])
    then [rm_from_row (explode h 0 []) letter] @ rm_letter t letter
    else [h] @ rm_letter t letter


let rec new_kb kb word idx =
	(* TODO *)
  if idx < String.length word
  then 
    if String.contains state.the_word word.[idx]
    then new_kb kb word (idx + 1)
    else new_kb (rm_letter kb word.[idx]) word (idx + 1)
  else kb  


  let rec fill_in_help blanks word i a =
    if i < String.length word
    then
      if word.[i] = state.the_word.[i]
      then fill_in_help blanks word (i+1) (a ^ (Char.escaped word.[i]))
      else fill_in_help blanks word (i+1) (a ^ (Char.escaped blanks.[i]))
    else a

  let fill_in_the_blanks blanks word =
	(* TODO *)
	fill_in_help blanks word 0 ""


let drop l idx =
  let p i _ = i > idx in
  List.filteri p l


let rm_dup strlst = 
	(* TODO *)
  let rm_dup_help i e = not (List.mem e (drop strlst (i+1))) in 
  List.filteri rm_dup_help strlst


let rec get_good_help l = 
  match l with
  | [] -> []
  | h::t ->
  if String.contains state.the_word h.[0]
  then [h] @ get_good_help t
  else get_good_help t

let get_good_letters word =
	(* TODO *)
	get_good_help (explode word 0 [])


let rec pcl_helper blanks good_letters =
	(* TODO *)
	match good_letters with
  | [] -> ""
  | h::t ->
    if String.contains state.the_word h.[0] && not(List.mem h t) && not(String.contains blanks h.[0])
    then h ^ " " ^ pcl_helper blanks t
    else pcl_helper blanks t 
	

let print_correct_letters blanks good_letters =
  begin
    print_endline "--------------------------------";
    print_endline ("-- letters in right spots: "^(space_out_word blanks));
    print_endline ("-- correct letters       : "^(pcl_helper blanks good_letters));
    print_endline "--------------------------------"
  end


let print_word str =
	print_endline (space_out_word str)


let print_word_history wh =
	begin
		print_word (List.nth wh 0);
		print_word (List.nth wh 1);
		print_word (List.nth wh 2);
		print_word (List.nth wh 3);
		print_word (List.nth wh 4);
		print_word (List.nth wh 5)
	end


let game_over_billboard =
  ["---------------";
   "-- GAME OVER --";
   "---------------"]

let not_valid_word_billboard =
  [ "----------------------------------------";
    "-- Not a valid word!					 --";
    "-- Reenter your guess(5 letter word): "]

let you_won_billboard =
  [ "-----------------";
    "-- YOU WON !!! --";
    "-----------------";
    ""]

let enter_your_guess_billboard =
  [ "";
    "= = = = = = = = = = = = = = =";
    "";
    "Enter your guess (5 letter word): "]

let print_hud state word_entered blnk gl = 
  if word_entered = state.the_word
  then
    begin
      List.iter print_endline you_won_billboard;
      List.iter print_word state.w_h; (* print word history *)
      failwith "END GAME"
    end
  else
    begin
      print_correct_letters blnk gl;
      print_endline "";
      List.iter print_word state.w_h; (* print word history *)
      print_endline "";
      List.iter print_word state.kb; (* print keyboard *)
      List.iter print_endline enter_your_guess_billboard
    end


let rec game_loop state blnk gl num_tries=
  if num_tries = 6
  then 
    begin
      List.iter print_endline game_over_billboard;
      failwith "END GAME"
    end
  else
    let s = read_line ()
    in let s_upper = String.uppercase_ascii s
    in 
    if is_word_valid s_upper
    then
      (let new_blnk = fill_in_the_blanks blnk s_upper
       in let new_gl = gl @ (get_good_letters s_upper)
       in let new_word_history = (List.mapi (fun i word -> if i=num_tries then s_upper else word) state.w_h)
       in let new_key_board = (new_kb state.kb s_upper 0)
       in let new_state = {state with
                           w_h=new_word_history; kb=new_key_board}
       in
       begin
         print_hud new_state s_upper new_blnk new_gl;
	 game_loop new_state new_blnk new_gl (num_tries+1)
       end)
    else
      (match s_upper with
       |"QUIT" | "EXIT" -> failwith "Quit"
       | _ ->
	 begin
	   List.iter print_endline not_valid_word_billboard;
	   game_loop state blnk gl num_tries
         end
      )


let () =
  begin
    print_hud state "_____" "_____" [];
    game_loop state "_____" [] 0
  end
