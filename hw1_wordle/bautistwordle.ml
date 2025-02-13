(*
WORDLE GAME
Author: Sean Bautista
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


let rec process_data src =
	(* TODO *)

  (* Capitalizes each letter in a word recursively *)
  match src with
  | [] -> []
  | h::t -> List.map String.uppercase_ascii h @ process_data t;;


let process_data_fold src =
	(* TODO *)

  (* Capitzlizes each letter in a word using fold *)
  List.fold_left (fun h t -> h @ List.map String.uppercase_ascii t) [] src;;


  (* Capitalizes all words in word_bank_src *)
let word_bank = process_data word_bank_src


let is_word_valid w =
	(* TODO *)

  (* If given word w is a member of word_bank, return true. Otherwise, return false *)
  List.mem w word_bank


let space_out_word s =
	(* TODO *)

  (* Turn s into a list of one letter strings, and concatenate " " to each value in the list. This converts the list back into a string *)
  String.concat " " (List.init (String.length s) (fun c -> String.make 1 (String.get s c)))


let rec rm_letter kb letter =
	(* TODO *)

  (* Removes one letter from keyboard recursively *)
    match kb with
    (* If kb is empty, there is nothing to iterate through, terminate *)
    | [] -> []

    (* Otherwise compare each character of the each "keyboard row". If the current value c matches letter, replace it with ' ' *)
    | h::t -> (String.map (fun c -> if c = letter then ' ' else c) h) :: rm_letter t letter;;


let rec new_kb kb word idx =
	(* TODO *)

  (* Creates new keyboard after a guess *)
  match kb with
  | [] -> []  
  | h::t -> 

    (* If index >= word length, return the same kb you had *)
    if idx >= String.length word
    then h::t 

    (* Otherwise, call rm_letter to remove the letter in the word you guessed from the keyboard *)
    else new_kb (rm_letter (h::t) (String.get word idx)) word (idx + 1)



let fill_in_the_blanks blanks word =
	(* TODO *)

  (* If current value in blanks already has a value, keep it. If the letter in word is correct, and in the same position as in the_word, replace _ with the letter, otherwise, print _ *)
  String.mapi (fun idx c -> if c != '_' then c else if (String.get word idx) = (String.get state.the_word idx) then (String.get word idx) else '_') blanks
	


let rm_dup strlst = 
	(* TODO *)

  (* Removes duplicates from a list of strings using fold *)
  List.fold_left 
  (* lst is a list to be built with the values from strlst*)
  (fun lst e -> 
  match List.mem e lst with
  (* If element e is already in lst, do nothing and keep the list as is. Otherwise, add e to the end of lst *)
  | true -> lst
  | false -> lst @ [e]
  )
  [] strlst


let get_good_letters word =
	(* TODO *)
  (* Create a list of usable letters from word *)

  (* After List.filter, we have a char list, so List.map (String.make 1) allows us to create a list of strings, where each string has length 1 *)
  List.map (String.make 1) 

  (* Turn word into a list of its letters, and apply List.filter to see if the_word contains each letter *)
  (List.filter (fun c -> String.contains state.the_word c) (List.init (String.length word) (fun i -> String.get word i)))
  
  



let pcl_helper blanks good_letters =
	(* TODO *)

  (* Turn good_letters into a string*)
  let use_letters = String.concat "" good_letters in

  (* Return list of usable letters *)
  let filtered_letters = get_good_letters use_letters in

  (* Remove any possible duplicates *)
  let unique_letters = rm_dup filtered_letters in

  (* Turn blanks into a list of 1 character strings so that it can be compared to unique_letters *)
  let blank_strlst = List.init (String.length blanks) (fun c -> String.make 1 (String.get blanks c)) in

  (* If a value is in unique_letters but not blank_strlst, create a list of these values and convert them into a string *)
  space_out_word (String.concat "" (List.filter (fun e -> not (List.mem e blank_strlst)) unique_letters));;

	

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