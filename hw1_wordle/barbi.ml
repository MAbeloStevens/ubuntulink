(*
WORDLE GAME
Author: James Barbi *)
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

let rec map f l =
  match l with
  | [] -> []
  | ( x :: xs ) -> ( f x )::( map f xs )
;;


let upperl' = List.map String.uppercase_ascii


let rec process_data src =
	match src with
  | [] -> []
  | h::t -> upperl' h @ process_data t


let rec process_data_fold src =
	List.fold_right (fun h acc -> upperl' h @ acc) src []


let word_bank = process_data word_bank_src


let is_word_valid w =
	let rec go_through_list w l =
    match l with
    | [] -> false
    | h::t -> if h = w then true else go_through_list w t
  in
  go_through_list w word_bank
;;


let rec is_letter_in_word letter word idx = 
  if String.length word = 0 || idx >= 5
    then false
    else
      if word.[idx] = letter
        then true
        else is_letter_in_word letter word (idx + 1)
;;


let rec space_out_word s =
	match s with
  | "" -> ""
  | _ -> (String.sub s 0 1) ^ " " ^ space_out_word (String.sub s 1 (String.length s - 1))
;;


let rec rm_letter_helper row letter =
  if row = ""
    then ""
    else
      let first = row.[0] in
      let rest = String.sub row 1 (String.length row - 1) in
      if first = letter
        then " " ^ rest
        else String.make 1 first ^ rm_letter_helper rest letter
;;

let rec rm_letter kb letter =
  match kb with
  | [] -> []
  | h::t -> rm_letter_helper h letter :: rm_letter t letter
;;


let rec new_kb kb word idx =
	if idx = 5
    then kb
    else
      if is_letter_in_word word.[idx] state.the_word 0
        then new_kb kb word (idx + 1)
        else new_kb (rm_letter kb word.[idx]) word (idx + 1)
;;


let rec fill_in_the_blanks blanks word =
	if String.length word = 0
    then ""
    else
      if blanks.[0] != '_'
        then String.make 1 blanks.[0] ^ fill_in_the_blanks (String.sub blanks 1 (String.length blanks - 1)) (String.sub word 1 (String.length word - 1))
        else
          if word.[0] = state.the_word.[5 - String.length word]
            then String.make 1 word.[0] ^ fill_in_the_blanks (String.sub blanks 1 (String.length blanks - 1)) (String.sub word 1 (String.length word - 1))
            else "_" ^ fill_in_the_blanks (String.sub blanks 1 (String.length blanks - 1)) (String.sub word 1 (String.length word - 1))
;;


let rm_dup strlst = 
  List.sort_uniq compare strlst
;;


let rec get_good_letters word =
  if String.length word = 0
    then []
    else
      if is_letter_in_word word.[0] state.the_word 0
        then [String.make 1 word.[0]] @ get_good_letters (String.sub word 1 (String.length word - 1))
        else get_good_letters (String.sub word 1 (String.length word - 1))
;;


let rec pcl_helper blanks good_letters =
	let better_letters = rm_dup good_letters in
  let remaining = List.filter (fun letter -> not (is_letter_in_word letter.[0] blanks 0)) better_letters in
  String.concat " " remaining
;;	


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
