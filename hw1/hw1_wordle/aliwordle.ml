(*
WORDLE GAME
Author: Sahana Ali 
Pledge: I pledge my honor that I have abided by the Stevens Honor System
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


let rec process_data src =   (*porcess word bank without fold*)
  let rec element_uppercase lst =  (*helper fun tconverts string to uppecase*)
    match lst with
    | [] -> [] (*container within container so like converts first element to uppercase --> recursively*)
    | h :: t -> String.uppercase_ascii h :: element_uppercase t
  in
  match src with
  | [] -> []
  | h :: t -> element_uppercase h @ process_data t

(* (b) Process data using fold *)
let process_data_fold src =
  let element_uppercase lst =
    (** accumlate uppercase words into a nww list*)
    List.fold_left (fun acc s -> acc @ [String.uppercase_ascii s]) [] lst
  in
  List.fold_left (fun acc sublist -> acc @ element_uppercase sublist) [] src
  (* TODO *)
    (*match src with
    | [] -> []
    | h::t -> (String.uppercase_ascii h) @ process_data t
	raise (Failure "Not Implemented") *)
    (*match src with
    | [] -> []
    | h :: t -> (tring.uppercase_ascii h) @ process_data t;  (*think about how fold works and try doing it by yourself, don't use map*)
  raise (Failure "Not Implemented") *)


let word_bank = process_data word_bank_src


let is_word_valid w =
	(* TODO *)
  let rec mem e l = 
    match l with 
    | [] -> false 
    | h :: t -> (h=e) || mem e t 
  in 
  mem w word_bank





let rec space_out_word s =
  if String.length s = 0 then  (*basecse string empty -> "", 2ndbase case 1char -> 1 chat *)
    ""  
  else if String.length s = 1 then 
    s  
  else 
    (* extracts 1st char in substring (container in conatainer), concate with space *)
    let first_char = String.sub s 0 1 in  
    let rest_of_string = String.sub s 1 (String.length s - 1) in 
    first_char ^ " " ^ space_out_word rest_of_string  
  

let rec rm_letter kb letter =
  (*Melody said the test case for #4 it's wrong it should be 
  # rm_letter state.kb 'D';; *)
  match kb with
  | [] -> []
  | h :: t -> 
    let track = String.map (fun c -> if c = letter then ' ' else c) 
    in track h :: rm_letter t letter 
	(* TODO *)


let rec new_kb kb word idx =
  if idx >= String.length word then kb  (* basese: stops ndex exceeds word length *)
  else 
    let letter = String.get word idx in
    
    let updated_kb = 
      (*Melody also told me use state.the_word*)
      (*Melody also told to test be #new_kb state.kb "WRONG" 0;; *)
      if String.contains (String.uppercase_ascii state.the_word) letter then kb  (* if letr in word, keep in kb *)
      else rm_letter kb letter  
    in
    new_kb updated_kb word (idx + 1)  (* rec to next leter*)


let fill_in_the_blanks blanks word =
  let rec helper idx =
    if idx >= String.length blanks then ""  
    else 
      let curr = blanks.[idx] in  (* curren chars in bnlamks*)
      if curr = '_' then  (*updates blmaks if still blanks*)
        if word.[idx] = state.the_word.[idx] then
          (*replace corr letters, e;se keep underscores if not corr matchs*)

          (String.make 1 word.[idx]) ^ (helper (idx + 1))  
        else

          "_" ^ (helper (idx + 1))  
      else 
        (String.make 1 curr) ^ (helper (idx + 1))  (* current aalr reavled letters *)
  in
  helper 0  (* helperfun at indx0 *)
    
    
let rm_dup strlst = 
	(* TODO *)
  (*List.fold_left(fun(rm_dups strlst, h::ht)) *)
  List.fold_left (fun acc x -> if 
    List.mem x acc then 
      acc else acc @ [x]) [] strlst;;


let get_good_letters word =
  let rec helper idx acc =
    (*helper func gets index of word --> so word[i] compare teh_word[i]*)
    (*if equal skip letter cuz alr carrt [pstopm]
      add letter in wromg postions
  else skip incorr letter*)
    if idx >= String.length word then acc  

    else 
      let letter = String.make 1 word.[idx] in  (* char to string *)
      if word.[idx] = state.the_word.[idx] then 
        helper (idx + 1) acc  
      (* skips corr-postionleters *)
      else if String.contains state.the_word word.[idx] then 
        (*keeps dups and retian orders*)
        helper (idx + 1) (acc @ [letter]) 
      else 
        helper (idx + 1) acc  (*ignore incorrect letters *)
  in
  helper 0 []
  


let pcl_helper blanks good_letters =
  let rec helper gl seen acc =
    (*helper func process lists og godo leters*)
    match gl with
    | [] -> acc  (*basecase: return acc string *)
    | h :: t -> 
      (*skips blaks or alradded *)
      if String.contains blanks h.[0] || List.mem h seen then 
        helper t seen acc
      else 
        (*appends wieht spacings*)
        helper t (h :: seen) (if acc = "" then h else acc ^ " " ^ h)  
  in
  helper good_letters [] ""
  
  
  


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
