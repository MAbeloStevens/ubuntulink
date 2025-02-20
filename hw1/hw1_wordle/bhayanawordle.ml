(*
WORDLE GAME
Author: Shubham Bhayana
I pledge my honor that I have abided by the stevens honor system.
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
  match src with
    [] -> []
  | h::t -> 
      List.map String.uppercase_ascii h @ process_data t
 
	


let process_data_fold src =
  List.map String.uppercase_ascii (List.fold_left (fun holder l -> holder @ l) [] src)
	


let word_bank = process_data word_bank_src


let is_word_valid w =
  List.fold_left(fun acc word -> acc || word = w ) false word_bank


let space_out_word s =
  String.init (String.length s * 2 - 1) (fun i -> if i mod 2 = 0 then s.[i / 2] else ' ')
  

		

  let rec rm_letter kb letter =
    match kb with
    | [] -> []
    | h :: t ->
        let new_row = String.init (String.length h) (fun i -> if h.[i] = letter then ' ' else h.[i]) in
        new_row :: rm_letter t letter
  
	


  let rec new_kb kb word idx =
      match idx >= String.length word with
      | true -> kb  
      | false ->
        let letter = word.[idx] in
        let updated_kb = 
          if String.contains state.the_word letter then kb 
          else rm_letter kb letter 
        in
        new_kb updated_kb word (idx + 1)
    
    
    
  
    
  


let fill_in_the_blanks blanks word =
	(* TODO *)
  let target_word = "OCAML" in  (* Hard-coded target word *)
  String.mapi (fun i c ->
    if c <> '_' then c  (* Keep already revealed letters *)
    else if word.[i] = target_word.[i] then word.[i]  (* Reveal correctly guessed letters at the correct index *)
    else '_'
  ) blanks


  let rm_dup strlst =
    let result = ref "" in
    String.iter (fun c ->
      if not (String.contains !result c) then
        result := !result ^ (String.make 1 c)
    ) strlst;
    !result
  
    let get_good_letters word =
        let the_word = "OCAML" in
        List.fold_left (fun acc letter ->
          if String.contains the_word letter then
            letter :: acc
          else
            acc
        ) [] (List.init (String.length word) (String.get word))
      
      
    
      
      
    
  
  
	


    let pcl_helper blanks good_letters =
        let target_word = "OCAML" in
      
        let is_in_wrong_spot idx letter =
          if letter = target_word.[idx] then false
          else String.contains target_word letter
        in
      
        let filtered_letters =
          List.filteri (fun idx letter ->
            is_in_wrong_spot idx letter
          ) good_letters
        in
      
        String.concat " " (List.map (String.make 1) filtered_letters)
      
  
      
  
  
	
	

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
