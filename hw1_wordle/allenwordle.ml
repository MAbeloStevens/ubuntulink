(*
WORDLE GAME
Author: Mikkail Allen
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
while true do
lines := String.uppercase_ascii (String.trim (input_line chan)) :: !lines
done; !lines
with End_of_file -> close_in chan;
List.rev !lines
let word_bank_src = read_file "word_bank.csv"

(*Converts a list of strings into a list of strings in uppercase*)
let process_data src =
	List.map String.trim src

let process_data_fold src =
	List.fold_left (fun acc row -> 
	(String.trim row) :: acc) [] src |> List.rev

let word_bank = process_data word_bank_src
	
(*Checks whether a word exists in the word bank*)
let is_word_valid w =
	List.mem w word_bank

(*Returns a new string where characters are separated by one space character*)
let space_out_word s =
  	String.concat " " (List.init (String.length s) 
	(fun i -> String.make 1 s.[i]))

(*Gven the current keyboard and a letter to remove, returns a new keyboard where
the letter is replaced by a space character*)
let rm_letter kb letter =
  	List.map (String.map (fun c -> 
	if c = letter 
	then ' ' 
	else c)) kb

(*Returns a new keyboard with letters from word that are not present in the_word removed from kb*)
let rec new_kb kb word idx =
  	if idx >= String.length word 
	then kb
  	else new_kb (rm_letter kb word.[idx]) word (idx + 1)

(*Addresses the letteres in the right spots*)
let fill_in_the_blanks blanks word =
  	String.init (String.length word) 
	(fun i -> if word.[i] = state.the_word.[i] 
	then word.[i] 
	else blanks.[i])

(*Removes duplicates from a list*)
let rm_dup strlst =
 	List.fold_left (fun acc x ->
    	match List.mem x acc with
    	| true -> acc
    	| false -> acc @ [x]) [] strlst


(*Addressed correct letters*)
let get_good_letters word =
  	List.filter (fun c -> String.contains state.the_word c) 
	(List.of_seq (String.to_seq word))
	|> List.map (String.make 1)
	

let pcl_helper blanks good_letters =
	String.concat " " (rm_dup good_letters)	

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
