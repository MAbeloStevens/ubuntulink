(*
WORDLE GAME
Author: Sarah Basil
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
  (* inner recursive helper to not change function definition *)
  let rec helper rows =
    match rows with
    | [] -> [] (*empty list base case*)
    | row :: rest ->
      match row with
      | [] -> helper rest
      | x :: _ ->
        (* x is the first string in row, uppercase, then recurse *)
        String.uppercase_ascii x :: helper rest
  in
  (* calling helper on src, to return list of uppercase words. *)
  helper src


let process_data_fold src =
	let reversed =
    List.fold_left
    (*fold function, takes func, initial list/accumulator, 
    and source to fold over*)
      (fun acc row ->
         match row with
         | [] -> acc
         | x :: _ -> (String.uppercase_ascii x) :: acc
      )
      []
      src
      in List.rev reversed (*reverse at end since cons adds to front*)


let word_bank = process_data word_bank_src


let is_word_valid w =
	(* need to check length is 5 AND membership in word_bank *)
  String.length w = 5
  && List.mem w word_bank (*using another list func to check membership*)


let space_out_word s =
	let len = String.length s in
  (* A helper function that processes from index i to the end of s. *)
  let rec helper i =
    if i >= len then
      ""  (* no characters left *)
    else if i = len - 1 then
      (* if last character -> just one-char string, no trailing space *)
      String.make 1 s.[i] (*to make string from character*)
    else
      (* cotherwise, current char + space + recurse on next index *)
      (String.make 1 s.[i]) ^ " " ^ helper (i + 1) (* ^ is string concat operator*)
  in
  helper 0 (* call with index starting at 0*)
		

(* HELPER: remove_in_str is helper func for rm_letter --> replaces 
every occurrence of c in s with ' '. *)
let rec remove_in_str s c =
  if s = "" then
    ""
  else
    (* take first character of s:*)
    let first = s.[0] in
    (*getting rest of string by 
    String.sub <start index> <characters you want>: *)
    let tail = String.sub s 1 (String.length s - 1) in
    if first = c then
      (* if it matches, use " " to replace, then recurse. *)
      " " ^ remove_in_str tail c
    else
      (* otherwise keep first char, recurse: *)
      String.make 1 first ^ remove_in_str tail c

let rec rm_letter kb letter =
	match kb with
  | [] -> [] (*empty list base case*)
  | row :: rest ->
      (*going row by row, removing in each row*)
      let new_row = remove_in_str row letter in
      new_row :: rm_letter rest letter



    
let rec new_kb kb word idx =
	if idx >= (String.length word) then
    (* base case: no more characters in guessed word *)
    kb
  else
    let ch = word.[idx] in
    (* checking if ch is in the hidden word: *)
    (*can check by String.contains <word> <char>: *)
    if (String.contains state.the_word ch) then
      (* if hidden word has ch, do NOT remove -> keep kb same *)
      new_kb kb word (idx + 1)
    else
      (* if hidden word does NOT have ch -> remove it from kb,
          recurse *)
      let kb' = rm_letter kb ch in
      new_kb kb' word (idx + 1)


let fill_in_the_blanks blanks word =
	let len = String.length blanks in

  let rec helper i =
    if i >= len then
      ""  (* base case: done building new string *)
    else
      let old_char = blanks.[i] in  (*getting old char*)  
      let guess_char = word.[i] in  (*getting guessed char*)
      let correct_char =
        (* checking if guess_char matches the hidden word in that position *)
        if guess_char = state.the_word.[i] then
          guess_char (*replace with guessed char*)
        else
          old_char (*otherwise keep the old char*)
      in
      (* convert the single char to 1-char string, recurse: *)
      String.make 1 correct_char ^ helper (i + 1)
  in
  helper 0 (*call with index starting at 0*)


let rm_dup strlst = 
	let reversed_no_dup =
    List.fold_left (*will use fold again*)
      (fun acc x ->
         if List.mem x acc then
           acc (*if member exists, don't include*)
         else
           x :: acc (*otherwise put it in*)
      )
      []
      strlst
  in
  (* because of x :: acc, final list is reversed...
     List.rev to reverse it back: *)
  List.rev reversed_no_dup


let get_good_letters word =
	let rec loop i acc =
    if i >= String.length word then
      acc
    else
      let c = word.[i] in
      (* if c is in the hidden word 'state.the_word', keep*)
      if String.contains state.the_word c then
        (* prepend single char string, preserve duplicates *)
        loop (i+1) (String.make 1 c :: acc)
      else
        loop (i+1) acc
  in
  (* could reverse, but pdf says order is irrelevant. *)
  loop 0 []


let pcl_helper blanks good_letters =
	(* gathering all letters from [blanks] that are not '_' *)
  let revealed =
    let rec gather i acc =
      if i >= String.length blanks then acc
      else
        let c = blanks.[i] in
        if c <> '_' then
          (* convert char to a 1-char string, store in acc: *)
          gather (i + 1) (String.make 1 c :: acc)
        else
          gather (i + 1) acc
    in
    gather 0 []
  in
  (* filter out any letters in [good_letters] not already in [revealed] *)
  let leftover = List.filter (fun ltr -> not (List.mem ltr revealed)) good_letters in
  (* remove duplicates from leftover: *)
  let unique_leftover = rm_dup leftover in
  (* join them with a space " " using concat *)
  String.concat " " unique_leftover
	

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
