(*
WORDLE GAME
Author:  Brayden Abo
Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

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

(* 
process_data : string list list -> string list
Conversts a list of strings into a list of strings in their uppercase form.
Recursive implementation
*)
let rec process_data src =
	match src with 
  | [] -> []
  | head :: remaining ->
    let uppercase_list = List.map String.uppercase_ascii head in 
    uppercase_list @ process_data remaining

(* 
process_data : string list list -> string list
Conversts a list of strings into a list of strings in their uppercase form.
Non-recursive implementation, uses List.fold
*) 
let process_data_fold src =
  List.fold_left (fun head rem -> head @ (List.map String.uppercase_ascii rem)) [] src


let word_bank = process_data word_bank_src

(*
is_word_valid : string -> bool  
Checks whether a word, w is in the wordbank
Utilizes List.mem which returns a boolean if the element uppercase_word is in set word_bank
*)
let is_word_valid w =
  let uppercase_word = String.uppercase_ascii w in
  List.mem uppercase_word word_bank

(*
space_out_word : string -> string
Returns a string where each characacter is spaced out
Recurssive approach that utilizes pattern matching.
*)
let rec space_out_word s =
  match String.length s with 
  | 0 -> "" (* Base case, string s is empty*)
  | 1 -> s (* If string is only 1 char, just return the character*)
  | _ -> String.sub s 0 1 ^ " " ^ space_out_word (String.sub s 1 (String.length s - 1)) (* If lentgh > 1, substring and add a space*)
		

(*
rm_letter : string list -> char -> string list
Returns a keyboard with a certain letter removed from the current keyboard layout.
Utilized recursion and pattern matching to remove the letter   
*)
let rec rm_letter kb letter =
  match kb with
  | [] -> [] (* If blank keybord, base case*)
  | head :: remaining ->
    let row = String.map (fun lett -> if lett = letter then ' ' else lett) head in (* Replace a letter if found with blank *)
    row :: rm_letter remaining letter (* Recursively call function on the next list, which is the next row of keyboard *)

(*
new_kb : string list -> string -> int -> string list  
Returns a new keybaord with the letters from inputted word that are not present in the_word
idx is used to track the index throughout recursive calls
*)
let rec new_kb kb word idx =
	if idx >= String.length word then kb (* Base case: If our index is greater than word*)
  else 
    let letter = String.get word idx in 
    if not (String.contains state.the_word letter) then (* If the char in word is not present in the_word*)
      new_kb (rm_letter kb letter) word (idx + 1) (* Use rm_letter to remove from keyboard and recursuvely call new_kb with removed letter *)
  else 
    new_kb kb word (idx + 1) (* If it is present, recurisvly call new_kb and increment index*)

(*
fill_in_the_blanks : string -> string -> strin   
Used to communicate letters in the right spots as user progresses through the game
*)
let rec fill_in_the_blanks blanks word =
  if String.length blanks = 0 then "" (* Base case*)
  else
    let first_blank = String.get blanks 0 in (*Fist character of the blanks, which is "progres bar"*)
    let first_char_in_word = String.get word 0 in (* First character in user inputted word *)
    let first_char_target = String.get state.the_word (String.length state.the_word - String.length blanks) in (*First characater in the_word *)
    let rem_blanks = if String.length blanks > 1 then String.sub blanks 1 (String.length blanks - 1) else "" in (* Substring of rest of blanks*)
    let rem_word = if String.length word > 1 then String.sub word 1 (String.length word - 1) else "" in (* Substring rest of inputted word *)

    let guess = (* Utilize pattern matching to determine if we should reveal the letter or keep it blank *)
      match first_blank with
      (* If there exists a blank '_' -> If character in guess matches the target word, then "reveal", else keep it hidden*)
      | '_' -> if first_char_in_word = first_char_target then first_char_target else '_' 
      | _ -> first_blank (* If there exists a character, then keep that character as it has been guessed already*)
    in
    (* Utilize String.make which creates string of length 1 with our guessed char, and concats it to the output of recursive call*)
    String.make 1 guess ^ fill_in_the_blanks rem_blanks rem_word

(*
rm_dup : ’a list -> ’a list
Removes all duplicates from a string  
*)
let rm_dup strlst = 
	List.sort_uniq compare strlst

(*
get_good_letters : string -> string list  
Returns a list of letters from the word that are present in the_word
*)
let get_good_letters word =
  let list_state_word = List.init (String.length state.the_word) (String.get state.the_word) in (* Create list of character in the_word*)
  let list_word = List.init (String.length word) (String.get word) in (*Create list of strings in word*)
  List.map (String.make 1) (List.filter (fun character -> List.mem character list_state_word) list_word)
  
(*
pcl_helper : string -> string list -> string
Helper function to display progress during the game   
*)
let pcl_helper blanks good_letters =
  let blanks_chars = List.map (String.make 1) (List.init (String.length blanks) (String.get blanks)) in (* Create list of chars in blanks*)
  let unique_good_letters = List.sort_uniq compare good_letters in (* Removes duplicates from good_letters*)
  let filtered_letters = List.filter (fun ch -> not (List.mem ch blanks_chars)) unique_good_letters in (* Filter character in good_letters already in blank_chars*)
  String.concat " " filtered_letters (* Concat the list of filtered_letters into one list of strings*)
	

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
