(*
WORDLE GAME
Dante
*)
(* We will convert everything to uppercase for reasons, with "String.uppercase_ascii str" *)
(* There is no check for input validity. In the actual game, the input can only be non-grayed-out letters; no non-letter characters, exactly 5 letters, and an actual word in the dictionary *)
(* *Note: We could add input check as an exercise. It's really optional. *)
(* Install opam package "csv" with "opam install csv" *)
#require "csv"


(* This part is like the "game state" if you will. Can zip w_h, kb, the_word into one game_state record *)
(* "Wordle_history" *)
(*6 tries. They start with 5 blanks/undercores('_') *)
let w_h = ["_____";"_____";"_____";"_____";"_____";"_____"]

(* "Keyboard" *)
(* Displays the alphabet using the keyboard layout. Letters will be removed as game goes on. *)	
(* *Note: The spaces are for display purposes. Just to look good. *)
let kb = ["QWERTYUIOP";" ASDFGHJKL";"  ZXCVBNM"]

(* this is the target word to be guessed *)(* We will convert everything to uppercase *)
let the_word = String.uppercase_ascii "ocaml"



(* load word bank. A csv file of 5 letter words, with 'ocaml' added. *)
(* The csv has 1 column *)
(* The type of word_bank is string list list. It looks like this [["ocaml"]; ["apple"]; ...] *)
let word_bank_src = Csv.load "word_bank.csv"


(* Flatten word_bank into a string list. I.e. From [["ocaml"]; ["apple"]; ...] to ["ocaml"; "apple"] *)
(* AND, turn every word to uppercase. *)
let process_data src =
	(* TODO *)


(* Part 2: Use fold *)
let process_data_fold src =
	(* TODO *)


let word_bank = process_data word_bank_src


(* Given a word w, check if w exists in word_bank. Return true if w is in word_bank, false otherwise. *)
let is_word_valid w =
	(* TODO *)


(* The following are functions to be implemented. *)
(* space the characters out with one space. Ex: "SPACE" -> "S P A C E" *)
(* this is for nicer displays *)
let space_out_word s =
	(* TODO *)
		

(* replaces the letter in the keyboard display with space *)
(* "kb" is a list of strings, "letter" is a char *)
(* *Note: since we do not check for input validity, this will be the only reference that the player has for which letters are good/bad/etc. *)
(* Use recursion *)
let rec rm_letter kb letter =
	(* TODO *)


(* if the word contains letter not present in "the_word", remove those from kb *)
(* starts from idx = 0 to 4 *)
(* "kb" is a list of strings, "word" is a string(assume 5 characters), "idx" is an int(starts from 0, goes to 5) *)
let rec new_kb kb word idx =
	(* TODO *)


(* replaces the "blanks" if characters in "word" are in "the_word" and in the same position *)
(* "blanks" are used to tell the player which letters exist in "the_word" and are in the right spot *)
(* EX: "_____" "XXAML" -> "__AML". assume the target word is "OCAML" *)
(* EX: "O____" "XXAML" -> "O_AML". assume the target word is "OCAML" *)
(* "blanks" and "word" are both strings *)
let fill_in_the_blanks blanks word =
	(* TODO *)


(* removes duplicates from a list *)
(* Do NOT use recursion *)
(* NOTE: rm_dup is practically identical to rm_letter *)
let rec rm_dup strlst = 
	(* TODO *)


(* get a list of the good letters from "word" that are in "the_word", regardless of position *)
(* EX: assume "OCAML" is the target word, "MLMLM" -> ["M";"L";"M";"L";"M"]. "XXXXX" -> [] *)
let get_good_letters word =
	(* TODO *)


(* this is to help the print function "print_correct_letters", in case the list of strings is empty *)
(* It removes letters already in "blanks", and also removed duplicates in the "good_letters" list. *)
(* The output is a string of letters, that are spaced out with 1 space character *)
(* EX: "OCA__" ["O";"C";"M";"L";"L"] -> "M L" *)
let pcl_helper blanks good_letters =
	(* TODO *)
	

(* "blanks" starts out as 5 underscores, ie. "_____". As player starts getting the letters at the correct position, the blanks are replaced with those letters, ex. "OC__L" *)
(* "good_letters" is a string list with all the letters that are in "the_word", but not in the right positions, ex. ["O";"C";"O";"L";"O"] *)
let print_correct_letters blanks good_letters =
	begin
		print_endline "--------------------------------";
		print_endline "-- letters in the right spots --";
		print_endline "--------------------------------";
		print_endline ("    "^(space_out_word blanks));
		print_endline "--------------------------------";
		print_endline "-- correct letters            --";
		print_endline "--------------------------------";
		print_endline ("    "^(pcl_helper blanks good_letters));
		print_endline "--------------------------------"
	end


(* prints a string with letters that are spaced out *)
let print_word str =
	print_endline (space_out_word str)


(* Prints the words guessed so far. "wh" is a list of all guesses, where remaining attemps are strings that are 5 blanks/underscores, ie. "_____" *)
let print_word_history wh =
	begin
		print_word (List.nth wh 0);
		print_word (List.nth wh 1);
		print_word (List.nth wh 2);
		print_word (List.nth wh 3);
		print_word (List.nth wh 4);
		print_word (List.nth wh 5)
	end


let print_hud word_history keyboard word_entered blnk gl = 
	if word_entered = the_word
	then
		begin
			print_endline "-----------------";
			print_endline "-- YOU WON !!! --";
			print_endline "-----------------";
			print_endline "";
			print_word_history word_history;
			print_endline "";
			failwith "END GAME"
		end
	else
		begin
			print_endline "-----------------";
			print_endline "-- W O R D L E --";
			print_endline "-----------------";
			print_endline "";
			print_correct_letters blnk gl;
			print_endline "";
			print_word_history word_history;
			print_endline "";
			print_word (List.nth keyboard 0);
			print_word (List.nth keyboard 1);
			print_word (List.nth keyboard 2);
			print_endline "";
			print_endline "= = = = = = = = = = = = = = =";
			print_endline "";
			print_endline "Enter your guess (5 letter word):";
			print_endline ""
		end


(* "gl": string list. good letters. letters that are in "the_word", but not in the right position *)
let rec game_loop word_history keyboard blnk gl num_tries=
	if num_tries = 6
	then 
		begin
			print_endline "---------------";
			print_endline "-- GAME OVER --";
			print_endline "---------------";
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
				in let new_word_history = (List.mapi (fun i word -> if i=num_tries then s_upper else word) word_history)
				in let new_key_board = (new_kb keyboard s_upper 0)
				in
					begin
						print_hud new_word_history new_key_board s_upper new_blnk new_gl;
						game_loop new_word_history new_key_board new_blnk new_gl (num_tries+1)
					end)
			else
				(* if word is not in word bank, prompt new input *)
				(* This also makes sure that inputs must be 5-characters long. *)
				begin
					print_endline "----------------------------------------";
					print_endline "-- Not a valid word!					 --";
					print_endline "-- Reenter your guess(5 letter word): --";
					print_endline "----------------------------------------";
					game_loop word_history keyboard blnk gl num_tries
				end


let () =
	begin
		print_hud w_h kb "_____" "_____" [];
		game_loop w_h kb "_____" [] 0
	end
