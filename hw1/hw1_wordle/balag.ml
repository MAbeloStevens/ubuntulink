(*
WORDLE GAME
Author: Gustavo Balaguera
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
  match src with
  | [] -> []
  | _ -> List.map (fun row -> String.uppercase_ascii (String.concat " " row)) src


let process_data_fold src =
  List.rev (List.fold_left (fun a row -> (String.uppercase_ascii (String.concat " " row)) :: a) [] src)


let word_bank = process_data word_bank_src


let is_word_valid w =
  if List.mem w word_bank
  then true
  else false


let space_out_word s = 
  let rec helper s = 
    match s with
    | [] -> ""
    | h::t -> (String.make 1 h)^" "^(helper t)
  in helper (List.init (String.length s) (String.get s))		
  
let rec rm_letter kb letter =
  match kb with
  | [] -> []
  | h::t -> 
    if (String.contains h letter)
    then (String.concat " " [String.sub h 0 (String.index h letter); String.sub h ((String.index h letter)+1) ((String.length h) - (String.index h letter) - 1)]) :: t
    else h :: (rm_letter t letter)


let rec new_kb kb word idx =
  if idx >= String.length word then kb
  else
    let letter = String.get word idx in
    if String.contains state.the_word letter then
      new_kb kb word (idx + 1)
    else
      new_kb (rm_letter kb letter) word (idx + 1)


let fill_in_the_blanks blanks word =
  let rec helper blnks wrd idx a =
    if idx >= String.length blnks then a
    else
      let new_char = if String.get blnks idx <> '_' then String.get blnks idx
                     else if String.get state.the_word idx = String.get wrd idx then String.get wrd idx
                     else '_'
      in helper blnks wrd (idx + 1) (a ^ String.make 1 new_char)
  in helper blanks word 0 ""

let rm_dup strlst = 
  List.rev (List.fold_left (fun a x -> if List.mem x a then a else x :: a) [] strlst)

let get_good_letters word =
  let rec helper wrd =
    match wrd with
    | [] -> []
    | h::t -> if String.contains state.the_word h
              then String.make 1 h::(helper t)
              else helper t
  in helper (List.init (String.length word) (String.get word))

  let pcl_helper blanks good_letters =
    let rec helper blnks gl acc =
      match gl with
      | [] -> acc
      | h::t ->
        if String.contains blnks (String.get h 0) then helper blnks t acc
        else helper blnks t (acc ^ h ^ " ")
    in helper blanks (rm_dup good_letters) ""
  
  let print_correct_letters blanks good_letters =
    begin
    print_endline "--------------------------------";
    print_endline ("-- letters in right spots: "^(space_out_word blanks));
    print_endline ("-- correct letters : "^(pcl_helper blanks good_letters));
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
    "-- Not a valid word! --";
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
    in let new_word_history = (List.mapi (fun i word -> if i=num_tries then
    s_upper else word) state.w_h)
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