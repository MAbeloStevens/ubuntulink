(*
WORDLE GAME
Author: Iva Buric
I pledge my honor that I have abided by the Stevens Honor System.
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

(* Helper function that capitalizes the letters of the string *)
let upper s = String.uppercase_ascii s

(* Helper function that checks whether a letter is in uppercase *)
let is_uppercase c = 'A' <= c && c <= 'Z'

(* Convertrs a list of lists of strings into a list of strings in uppercase *)
let process_data src = List.map upper (List.concat src)

(* Convertrs a list of lists of strings into a list of strings in uppercase - using fold *)
let process_data_fold src = List.map upper (List.fold_right (@) src [])

let word_bank = process_data word_bank_src

(* Checks whether the word exists in the word bank(and if it is in uppercase) *)
let is_word_valid w =
  String.length w = 5 &&
  String.for_all is_uppercase w &&
  List.exists (fun word -> word = w) ( process_data word_bank_src)

(* 
  Helper function that turns a string into a list of single letters
   (as in most cases I decided to work with chars) 
*)
let string_to_char_list s = List.of_seq (String.to_seq s)

(* Helper function that turns a list of characters into a string *)
let char_list_to_string cl = String.of_seq (List.to_seq cl)

(* Returns a lstring where characters are separated by spaces *)
let space_out_word s = String.concat " " (List.map (String.make 1) (string_to_char_list s))
		
(* Helper function that replaces a letter in a string with a space *)
let rec letter_to_space letter l =
  match l with
  | [] -> []  
  | h::t -> 
      if h = letter 
        then ' '::letter_to_space letter t
      else h::letter_to_space letter t

(* Returns a new keyboard where the letter is replaced by a space *)
let rm_letter kb letter =
  List.map (fun s -> char_list_to_string (letter_to_space letter (string_to_char_list s))) kb


(* Returns a new keyboard with letters from word that are not present in the_word removed from kb *)
let rec new_kb kb word idx =
  if idx = 5 then kb  
  else
    let letter = word.[idx] in
    if not (String.contains state.the_word letter) 
      then new_kb (rm_letter kb letter) word (idx + 1)
    else
      new_kb kb word (idx + 1)

(* Returns a string of characters that are in the right spot by replacing the blanks *)
let fill_in_the_blanks blanks word =
  String.mapi(fun i c -> 
    if c <> '_' then c  
    else 
      if word.[i] = state.the_word.[i] 
        then word.[i]  
      else '_') blanks

(* Removes the duplicated from a list of characters *)
let rm_dup strlst = List.fold_left (fun f n -> if List.mem n f then f else f @ [n]) [] strlst

(* Returns the list of correctly guessed letters *)
let get_good_letters word =
  let l1 = string_to_char_list word in
  let l2 = string_to_char_list state.the_word in
  let good_letters = List.filter (fun e -> List.mem e l2) l1 in
  List.map (String.make 1) good_letters

(*
 Helper function that returns a string composed of spaced out letters that are present in the_word,
 but not present in the string blanks 
*)
let pcl_helper blanks good_letters =
  let blank_chars = string_to_char_list blanks in
  let letters = List.filter 
  (fun letter -> not (List.mem (letter.[0]) blank_chars)) good_letters in
  let unique = rm_dup letters in String.concat " " unique 

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
