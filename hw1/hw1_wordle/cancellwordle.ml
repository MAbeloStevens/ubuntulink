(*
WORDLE GAME
Author: Aidan Cancelliere
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
    done; 
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
let word_bank_src = read_file "word_bank.csv"


let process_data src = 
  List.map (fun [wrd] -> String.uppercase_ascii wrd) src

let process_data_fold src =
  List.fold_right (fun inner_list acc ->
    match inner_list with
    | [] -> "" :: acc 
    | [wrd] -> String.uppercase_ascii wrd :: acc) src []  

let word_bank = process_data word_bank_src


let is_word_valid w =
  List.mem w word_bank;;

(* String/List.to_seq = Converts a String/List into a sequence of characters 
   String/List.of_seq = Converts a sequence into a String/List*)

(*
   First, I separated the word s into a sequence of characters, then turned that sequence into a list.
   Then, I used that list to take each character, create a copy of it, and concatenate it (^) with a space.
   Lastly, I took the list of spaced characters and condensed it back into a single string.
*)
let space_out_word s =
  let char_list = List.of_seq (String.to_seq s) in
  let spaced_chars = List.map (fun l -> String.make 1 l ^ " ") char_list in
  String.concat "" spaced_chars;;

(* Iterates over each row of the keyboard and replaces a given letter with a space using String.map *)
let rm_letter kb letter =
  List.map (fun row ->
    String.map (fun kb_char -> if kb_char = letter then ' ' else kb_char) row
  ) kb;;

(* Recursively updates the keyboard by removing letters not found in the target word *)
let rec new_kb kb word idx =
  let length = (String.length word) in
    if idx >= length then kb (* Base case: If all letters have been checked, return kb *)
    else 
      let the_word_char_list = List.of_seq (String.to_seq state.the_word) in
      if List.mem word.[idx] the_word_char_list 
      then new_kb kb word (idx + 1)  (* skip the letter if it's not in the_word *)
      else new_kb (rm_letter kb word.[idx]) word (idx+1);; (* Remove the letter *)

(*
   First, I converted each input string (blanks, word, and the_word) into a list of characters using List.of_seq.
   Then, I combined the guessed word with the target word and blanks into a list of tuples.
   I mapped over this list, checking if each guessed letter matches the correct word's letter.
   If it matched, I replaced the blank with the correct letter; otherwise, I kept the blank.
   Finally, I converted the updated list back into a string using String.of_seq.
*)
let fill_in_the_blanks blanks word =
  let the_word_chars = List.of_seq (String.to_seq state.the_word) in
  let word_chars = List.of_seq (String.to_seq word) in
  let blanks_chars = List.of_seq (String.to_seq blanks) in

  let indexed_word = List.combine (List.combine word_chars the_word_chars) blanks_chars in

  let updated_blanks = 
    List.map (fun ((a1, a2), b) -> if a1 = a2 then a2 else b) indexed_word
  in

  String.of_seq (List.to_seq updated_blanks)
      
(* Uses fold_right to iterate through the list and remove duplicates while keeping the first occurrence *)
let rm_dup strlst =
  List.fold_right (fun x acc -> if List.mem x acc then acc else x :: acc) strlst []

(*
   First, I converted the target word and guessed word into lists of characters.
   Then, I used filteri, which provides both the element and its index, to check:
   1. If the guessed letter is different from the correct letter at that position in the target word.
   2. If the guessed letter is present somewhere in the target word.
   After filtering, I applied List.map separately to convert each character to a string using String.make 1 instead of chaining it with the pipeline operator.
*)
let get_good_letters word =
  let the_word_chars = List.of_seq (String.to_seq state.the_word) in
  let word_chars = List.of_seq (String.to_seq word) in

  let filtered_chars = List.filteri (fun i w -> w <> List.nth the_word_chars i && List.mem w the_word_chars) word_chars in
  List.map (String.make 1) filtered_chars  (* Convert each char to a string *)
	

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
