(*
WORDLE GAME
Author: Christopher Bernard
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
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



let word_bank_src = Csv.load "word_bank.csv"

(* turns file info into list and capitalizes each word recursively *)
let rec process_data src =
  (* recursively matches src, capitalizing the head of the head of src and then continuing *)
  match src with
  | [] -> []
  | h::t -> (String.uppercase_ascii (List.hd h) ) :: process_data t

(* turns file info into list and capitalizes each word using fold*)
let process_data_fold src =
  (* concatenates the inner lists, uses map to capitalize each word *)
  List.map (String.uppercase_ascii) (List.flatten src)

let word_bank = process_data word_bank_src


(* checks if a word (w) is in the word bank *)
let is_word_valid w =
  (* folds a list containing truth values for if a word in word_bank is equal to the given word *)
	List.fold_right (||) (List.map (fun word -> w=word) word_bank) false


(* helper function for fold, adds a space after every letter *)
let space_out_word_helper str char = str ^ String.make 1 char ^ " "

(* adds spaces between each letter of a string *)
let space_out_word s =
  (* folds with helper function and trims whitespace*)
	String.trim (String.fold_left (space_out_word_helper) "" s)
		

(* removes letter from keyboard (kb) *)
let rec rm_letter kb letter =
	match kb with
	| [] -> []
	| h::t ->
		if (String.contains h letter) (* checks if row contains letter *)
		then (String.map (fun c -> if (c = letter) then ' ' else c) h) :: rm_letter t letter (* if it does, replace it with a space and add to output *)
		else h :: rm_letter t letter (* add row back to keyboard otherwise *)


(* returns kb where letters in word that aren't in solution are removed*)
let rec new_kb kb word idx =
	if(idx < String.length word) (* checks if at valid index *)
    then
      if String.contains state.the_word (String.get word idx) (* checks if the_word contains current letter *)
      then new_kb kb word (idx+1) (* continue on, since the letter is used in solution *)
      else new_kb (rm_letter kb (String.get word idx)) word (idx+1) (* remove letter from kb *)
    else
      kb (* return the keyboard, done iterating *)


(* helper function for fill_in_the_blanks that has an index parameter*)
let rec fill_blanks_helper blanks word idx =
  if (idx < 5) (* checks if index is a valid value *)
  then
    if (String.get word idx) = (String.get state.the_word idx) (*checks if letters at same idx are the same*)
    then String.make 1 (String.get state.the_word idx) ^ fill_blanks_helper blanks word (idx+1) (* if same, add to output *)
    else String.make 1 (String.get blanks idx) ^ fill_blanks_helper blanks word (idx+1) (* adds original char from blank to output otherwise *)
  else
    ""

(* returns string with the letters in word in the right locations *)
let fill_in_the_blanks blanks word =
  fill_blanks_helper blanks word 0 (* calls helper function starting at index 0 *)
	

(* removes duplicates using fold *)
let rm_dup strlst = 
  (* anonymous function checks if item exists already in output, discarding it if it does *)
	List.fold_right (fun item list -> if (List.mem item list) then list else item::list) strlst []


(* returns the characters of word that are in the solution *)
let get_good_letters word =
  (* anonymous function checks if currnet char is in word, if so include it in the output, discarding it otherwise*)
	String.fold_right (fun char str -> if(String.contains state.the_word char) then (String.make 1 char)::str else str ) word []


(* helper function for pcl_helper *)
let rec pcl_helper_helper blanks good_letters =
  match good_letters with
  | [] -> "" (* done processing *)
  | h::t ->
    if (String.contains blanks (String.get h 0)) (* checks if current letter is in blanks *)
    then pcl_helper_helper blanks t (* if so, don't add it to the output *)
    else h ^ pcl_helper_helper blanks t (* prepend letter otherwise *)

(* returns string of spaced out letters in the solution but not in blanks*)
let pcl_helper blanks good_letters =
  (* calls pcl_helper_helper with duplicates removed from good_letters and spaces it out *)
  space_out_word (pcl_helper_helper blanks (rm_dup good_letters))
  
	

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
