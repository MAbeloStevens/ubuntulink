(*
WORDLE GAME
Author: Jacob Barbulescu
  I pledge my Honor that I have abided by the Stevens Honor System.
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

(** Replaces string[index] with newChar. 
    If index is invalid it just returns the original string *)
let replaceChar string index newChar =
  if index >= 0
    then String.sub string 0 index ^ String.make 1 newChar ^ String.sub string (index+1) (String.length string - (index+1))
    else string

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

let rec process_data_helper src =
  match src with
  | [] -> []
  (* Appends each list to the next *)
  | h::t -> h @ process_data_helper t

let process_data src =
  List.map (String.uppercase_ascii) (process_data_helper src)

let process_data_fold src =
	List.map (String.uppercase_ascii) (List.concat src)


let word_bank = process_data word_bank_src


let is_word_valid w =
  (* Simply checks if the given word is in the valid word bank *)
	List.mem w word_bank


let rec space_out_word s =
  (* In each call, take the first letter of s, add a space after it, then move on *)
	if String.length s = 0
    then ""
    else String.sub s 0 1 ^ " " ^ space_out_word (String.sub s 1 ((String.length s) - 1))
		

let rec indexOfHelper char string index =
  (* If we leave the string, return -1 (not found) *)
  if index >= String.length string
    then -1
    else
    (* If we do have a match, return the current index. Else, continue *)
      if Char.equal (String.get string index) char
        then index
        else indexOfHelper char string (index+1)

(** Finds the index of a given char in a string (Returns -1 if not found) *)
let rec indexOf char string =
  indexOfHelper char string 0

let rec rm_letter kb letter =
  (* Finds where letter is in kb and replaces it with an empty space *)
	match kb with
  | [] -> []
  | h::t -> (replaceChar h (indexOf letter h) ' ')::rm_letter t letter


let rec new_kb kb word idx =
	if idx >= String.length word
    then kb
    else new_kb (rm_letter kb (String.get word idx)) word (idx+1)


let rec fill_in_the_blanks_helper blanks word index =
  if index >= String.length blanks then "" else
  (* If the letter in a word directly matches the one in the target word, then overwrite that spot in blank with that letter *)
  (if Char.equal (String.get state.the_word index) (String.get word index)
    then (String.sub word index 1)
    else (String.sub blanks index 1)) ^ fill_in_the_blanks_helper blanks word (index+1)

let fill_in_the_blanks blanks word =
	fill_in_the_blanks_helper blanks word 0


let rm_dup strlst = 
  (* This sorting function also automatically removes duplicates *)
	List.sort_uniq (String.compare) strlst


let rec get_good_letters_helper word index =
  if index >= String.length word
    then []
    else
      (* If the current letter is in the target word, add it to the good letter list *)
      if String.contains state.the_word (String.get word index)
        then (String.sub word index 1)::get_good_letters_helper word (index+1)
        else get_good_letters_helper word (index+1)

let get_good_letters word =
	get_good_letters_helper word 0


let rec pcl_helper blanks good_letters =
  match (rm_dup good_letters) with
  | [] -> ""
            (* If the letter is good but in the wrong spot, add it to this string here *)
  | h::t -> if ((String.contains state.the_word (String.get h 0)) && (not (String.contains blanks (String.get h 0))))
              then h^" "^(pcl_helper blanks t)
              else (pcl_helper blanks t)
	

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
