(*
WORDLE GAME
Author: Jaran Binning
*)
(* Install opam package "csv" with "opam install csv" *
#require "csv"*)


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
  done; !lines
  with End_of_file ->
  close_in chan;
  List.rev !lines
  
let word_bank_src = read_file "word_bank.csv"


let rec process_data src =
  match src with
	| [] -> []
	| h::t -> String.uppercase_ascii (String.concat "" h) :: process_data t


  let process_data_fold src =
    List.fold_right (fun f x -> String.uppercase_ascii (String.concat "" f) :: x) src []
  

let word_bank = process_data_fold word_bank_src


let is_word_valid w =
	List.mem w word_bank


let space_out_word s =
  String.concat " " (List.init (String.length s) (fun i -> String.make 1 s.[i]))
		

let rec rm_letter kb ch =
  match kb with
  | [] -> []
  | h ::t ->
      let new_row = String.map (fun c -> if c = ch then ' ' else c) h in
      new_row :: rm_letter t ch


let rec new_kb kb word idx =
  if idx >= String.length word 
    then kb
    else
      let kb' = if not (String.contains state.the_word word.[idx]) 
        then rm_letter kb word.[idx] 
        else kb in
    new_kb kb' word (idx + 1)

let rec fill_in_the_blanks_helper blanks guess idx =
  if idx >= String.length blanks then ""
  else
    let current_letter =
      if blanks.[idx] <> '_' 
        then blanks.[idx]
        else if guess.[idx] = state.the_word.[idx] 
              then guess.[idx]
              else '_' in
    String.make 1 current_letter ^ fill_in_the_blanks_helper blanks guess (idx + 1)


let fill_in_the_blanks blanks word =
	fill_in_the_blanks_helper blanks word 0


let rm_dup strlst = 
	List.fold_right (fun x l -> if List.mem x l then l else x :: l) strlst []


let rec get_good_letters_helper word idx l =
  if idx >= String.length word 
    then l
    else
      let c = String.make 1 word.[idx] in
      let new_l = if String.contains state.the_word word.[idx] then c :: l else l in
      get_good_letters_helper word (idx + 1) new_l

let get_good_letters word =
	get_good_letters_helper word 0 []


let rec pcl_helper blanks good_letters =
	space_out_word (String.concat "" (List.filter (fun c -> not (String.contains blanks c.[0])) good_letters))

	

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
