(*
WORDLE GAME
Author: Jennifer Alexander
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


let process_data src =
    let upperLists = List.map( fun lst -> 
      match lst with
      |[] -> []
      |_ -> List.map String.uppercase_ascii lst
    ) src 
    in 
    List.flatten upperLists



let process_data_fold src =
	List.fold_left( fun result lst ->
    result @ (List.map String.uppercase_ascii lst)
  ) [] src


let word_bank = process_data word_bank_src


let is_word_valid w =
  (*
  check if w is a member of word bank list
     *)
  List.mem w word_bank


let space_out_word s =
  (* 
  Take given string and turn into a sequence of chars.
  Take the sequence of chars and turn into a list of chars.
  Iterate through the list with match and convert each element to a string,
  concat each element together into one string with a " ", unless its the last element
     *)
  let charList = List.of_seq(String.to_seq s) in 
  let rec spacedString lst = 
    match lst with
    |[]->""
    |[x]-> String.make 1 x
    |x::t -> (String.make 1 x) ^ " " ^ spacedString t
  in
  spacedString charList
		

let rec rm_letter kb letter =
	(* 
     recursively process list
     then use String.map to check each letter in each string element of kb list
     if char matches letter, set var newSpace = " ". if not, keep it the same and 
      continue processing list 
  *)
    match kb with
    | [] -> []
    | h::t -> 
        let newSpace = String.map (fun x -> if x=letter then ' ' else x) h in
        newSpace :: rm_letter t letter


let rec new_kb kb word idx =
	(* 
  take input word and for each char not present in target, remove from kb
     if char not in target, rm_letter*)
    let targetList = List.of_seq(String.to_seq state.the_word) in
     let word_suffix = String.sub word idx (String.length word - idx) in
    let wordList = List.of_seq(String.to_seq word) in
     List.fold_left (fun currkb lett -> 
        if List.mem lett targetList
          then currkb
      else rm_letter currkb lett
      ) kb wordList




let fill_in_the_blanks blanks word =
  let blanksList = List.of_seq (String.to_seq blanks) in
  let wordList = List.of_seq(String.to_seq word) in
  let targetList = List.of_seq(String.to_seq state.the_word) in
  let resultList1 = 
      List.map2 ( fun w t ->
       if w = t then w 
        else '_'
        )  wordList targetList in
  let resultList2 = 
      List.map2(fun r1 b ->
        if b <> '_' then b
        else r1
        ) resultList1 blanksList in 
      String.of_seq(List.to_seq resultList2) 


let rm_dup strlst = 
  (*create a new list by processing each element in strlst (with foldLeft)
     and checking if its a member of the new list we are creating.
      if it is not already present in the new list, add it to the new list.
      else, leave the list as is and continue processing*)
    let listRev = List.fold_left(fun newL curr ->
      if List.mem curr newL then newL
      else curr :: newL
      ) [] strlst in
      List.rev listRev



let get_good_letters word =
	(* 
  convert the target word and the input word into a list to be processed
  use fold_left to process wordList and see which characters in word are 
  also in the target word. if a character from word is present in the target word
  then add that character to the resulting list. if not, do nothing and continue
  processing
  *)
  let targetList = List.of_seq(String.to_seq state.the_word) in
  let wordList = List.of_seq(String.to_seq word) in 
    List.fold_left (fun result w ->
      if List.mem w targetList
      then result @ [String.make 1 w]
      else result
      ) [] wordList


let pcl_helper blanks good_letters =
  let blanksList = List.of_seq(String.to_seq blanks) in
  let finalList = List.fold_left( fun result g ->
      if List.mem (String.get g 0) blanksList 
        then result
    else result @ [g; " "]
  )[] good_letters in
    let final2 = rm_dup finalList in
    String.concat "" final2

	

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
