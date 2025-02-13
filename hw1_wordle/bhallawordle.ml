(*
WORDLE GAME
Author: Jason Bhalla
I pledge my honor that I have abided by the Stevens Honor System.
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


(* Helper function: Convert every string in a list to all uppercase *)
let rec uppercase_list l =
  match l with
  | [] -> []
  | h::t ->
    String.uppercase_ascii h :: uppercase_list t

(* Helper function: Goes through a list of lists of strings and converts each list to uppercase strings *)
let rec uppercase_list_lists src =
  match src with
  | [] -> []
  | h::t ->
    uppercase_list h :: uppercase_list_lists t

let rec process_data src =
  (* TODO *)
  match src with
  | [] -> []
  | h::t ->
    uppercase_list h @ process_data t


let process_data_fold src =
	(* TODO *)
	List.map (fun e -> String.uppercase_ascii e) (List.fold_left (fun l1 l -> l1 @ l) [] src)

let word_bank = process_data word_bank_src


let is_word_valid w =
	(* TODO *)
	List.mem w word_bank

(* Helper function: Spaces out the letters *)
let rec space_out s i n =
  match i with
  | _ when i = n - 1 -> String.make 1 (String.get s i)
  | _ -> (String.make 1 (String.get s i)) ^ " " ^ space_out s (i + 1) n
    

let space_out_word s =
	(* TODO *)
  let n = String.length s in
  match n with
  | 0 -> ""
  | _ -> space_out s 0 n
		

(* Helper function: Removing the letter from the string kb starting at i *)
let rec replace_string kb i letter =
  match i with
  | _ when i >= String.length kb -> ""
  | _ ->
    match kb.[i] with
    | a when a = letter -> " " ^ replace_string kb (i + 1) letter
    | a -> String.make 1 a ^ replace_string kb (i + 1) letter


let rec rm_letter kb letter =
	(* TODO *)
  match kb with
  | [] -> []
  | head :: tail -> replace_string head 0 letter :: rm_letter tail letter
	
(* Helper function: Checks if a letter is in the target word *)
let rec letter_in_word_getter letter i =
  match i with
  | _ when i >= String.length state.the_word -> false
  | _ -> if state.the_word.[i] = letter then true else letter_in_word_getter letter (i + 1)

(* Helper function: Checks if a letter is in the target word *)
let letter_in_word letter =
  letter_in_word_getter letter 0

let rec new_kb kb word idx =
	(* TODO *)
	match idx with
  | _ when idx >= String.length word -> kb
  | _ ->
      let letter = word.[idx] in
      match letter_in_word letter with
      | true -> new_kb kb word (idx + 1)
      | false -> new_kb (rm_letter kb letter) word (idx + 1)


(* Helper function: Updating the blanks *)
let rec fill_blanks blanks word i =
  match i with
  | _ when i >= String.length blanks -> ""
  | _ ->
    match blanks.[i] with
    | '_' ->
      (match word.[i] = state.the_word.[i] with
      | true -> String.make 1 word.[i] ^ fill_blanks blanks word (i+1)
      | false -> "_" ^ fill_blanks blanks word (i+1))
    | b -> String.make 1 b ^ fill_blanks blanks word (i+1)

let fill_in_the_blanks blanks word =
	(* TODO *)
	fill_blanks blanks word 0


let rm_dup strlst = 
	(* TODO *)
	List.fold_right (fun x l ->
    match List.mem x l with
    | true -> l
    | false -> x :: l
  ) strlst[]

(* Helper function: Creates list of letters present in the_word *)
let rec get_letters word i =
  match i with
  | _ when i >= String.length word -> []
  | _ ->
    match word.[i] with
    | a ->
      match letter_in_word a with
      | true -> String.make 1 a :: get_letters word (i+1)
      | false -> get_letters word (i+1)

let get_good_letters word =
	(* TODO *)
	get_letters word 0

(* Helper function: Checks if a letter is in a string *)
let rec letter_in_string letter s i =
  match i with
  | _ when i >= String.length s -> false
  | _ ->
    match s.[i] = letter.[0] with
    | true -> true
    | false -> letter_in_string letter s (i+1)

(* Helper function: Removes letters that appear in blanks *)
let rec filter_letters blanks letters =
  match letters with
  | [] -> []
  | h::t ->
    match letter_in_string h blanks 0 with
    | true -> filter_letters blanks t
    | false -> h :: filter_letters blanks t

let pcl_helper blanks good_letters =
	(* TODO *)
	String.concat " " (rm_dup (filter_letters blanks good_letters))
	

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
