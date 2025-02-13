(*
WORDLE GAME
Author: Vidhur Busannagari
Pledge: I pledge my honor that I have abided by the stevens honor system - VB
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


(* Recursively processes a list of lists of strings, converting each string to uppercase *)
let rec process_data src =
  match src with (* Base case: empty list *)
  | [] -> []
  | row :: rest ->
      (* Helper function to process individual row *)
      let rec process_row r =
        match r with
        | [] -> [] (* Base case: empty row *)
        | word :: ws -> String.uppercase_ascii word :: process_row ws (* Convert word to uppercase and process rest *)
      in
      process_row row @ process_data rest (* Combine processed row with rest of processed data *)


let process_data_fold src =
  List.fold_left (fun acc row -> (* Outer fold: accumulates results from each row *)
      let row_upper =
        List.fold_right (fun word lst -> (* Inner fold: processes each word in current row *)
            String.uppercase_ascii word :: lst (* Convert to uppercase and add to list *)
          ) row [] in
      acc @ row_upper (* Append processed row to accumulated result *)
    ) [] src

(* Global word bank created from processing source data *)
let word_bank = process_data word_bank_src

(* Checks if a word exists in the word bank *)
let is_word_valid w =
  List.mem w word_bank

(* Adds spaces between characters in a strings *)
let space_out_word s =
  let rec aux i acc = (* Helper function to process each character *)
    if i >= String.length s then acc (* Base case: reached end of string *)
    else aux (i+1) (acc ^ (String.make 1 s.[i] ^ " ")) (* Recursive case: *)
  in
  aux 0 ""   (* Start with empty string at index 0 *)

(* Removes a letter from the keyboard display by replacing it with space *)
let rec rm_letter kb letter =
  match kb with
  | [] -> [] (* Base case: empty keyboard *)
  | row :: rest ->  (* Process each row of keyboard *)
      let len = String.length row in
      let rec replace i = (* Helper to process each character in row *)
        if i >= len then "" (* Base case: end of row *)
        else
          let c = row.[i] in (* Get current character *)
          let new_c = if c = letter then ' ' else c in (* Replace if matches *)
          String.make 1 new_c ^ replace (i+1)
      in
      (replace 0) :: rm_letter rest letter  (* Process rest of rows *)

(* Updates keyboard display based on guessed word *)
let rec new_kb kb word idx =
  if idx >= String.length word then kb (* Base case: processed all letters *)
  else
    let letter = word.[idx] in (* Get current letter *)
    let kb' =
      if not (String.contains state.the_word letter) then (* If letter not in target *)
        rm_letter kb letter (* Remove from keyboard *)
      else
        kb (* Keep keyboard as is *)
    in
    new_kb kb' word (idx+1) (* Process next letter *)

(* Updates display of correctly guessed letters *)
let fill_in_the_blanks blanks word =
  let target = state.the_word in
  let len = String.length target in
  let rec aux i acc = (* Helper to process each position *)
    if i >= len then acc (* Base case: processed all positions *)
    else
      let b = blanks.[i] in (* Current blank state *)
      let guess = word.[i] in (* Guessed letter *)
      let target_char = target.[i] in (* Target letter *)
      let new_char =
        if b <> '_' then b (* Keep already revealed letters *)
        else if guess = target_char then target_char (* Reveal correct guesses *)
        else '_' (* Keep incorrect guesses hidden *)
      in
      aux (i+1) (acc ^ (String.make 1 new_char)) (* Build result string *)
  in
  aux 0 ""

(* Removes duplicate elements from a list using fold *)
let rm_dup lst =
  List.fold_left 
  (fun acc x -> (* For each element x: *)
    if List.mem x acc then acc (* If already in accumulator, skip *)
    else acc @ [x]) (* Otherwise, add to accumulator *)
    [] lst (* Start with empty accumulator *)

(* Gets list of letters from word that appear in target word *)
let get_good_letters word =
  let target = state.the_word in
  let rec aux i acc = (* Helper to process each letter *)
    if i >= String.length word then acc (* Base case: processed all letters *)
    else
      let c = word.[i] in (* Current letter *)
      let acc' = if String.contains target c then acc @ [String.make 1 c] else acc in (* Add if in target or Skip if not*)
      aux (i+1) acc'
  in
  aux 0 []

(* Helper function for displaying correct letters not yet in right position *)
let pcl_helper blanks good_letters =
  let filtered = List.filter (fun letter_str -> (* Remove letters that are already correctly placed *)
      let letter = letter_str.[0] in
      not (String.contains blanks letter)
    ) good_letters
  in
  let unique = rm_dup filtered in  (* Remove duplicates *)
  let rec join lst = (* Helper to join with spaces *)
    match lst with
    | [] -> "" (* Base case: empty list *)
    | [x] -> x  (* Single element *)
    | x :: xs -> x ^ " " ^ join xs  (* Multiple elements *)
  in
  join unique
	

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