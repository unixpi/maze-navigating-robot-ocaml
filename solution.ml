let current_pos = ref (0,0)

let pos_visited = ref [(0,0)]

let letters_and_locations = ref []

let moves_made_so_far = ref []

let letters_caught = ref [] 

let update_current_pos p = (fun c -> match c , !p with
				     | 'U' , (a,b) -> p := (a,b+1); p
				     | 'D' , (a,b) -> p := (a,b-1); p
				     | 'R' , (a,b) -> p := (a+1,b); p
				     | 'L' , (a,b) -> p := (a-1,b); p)
				
let update_pos_visited p = (fun cp -> (p := (cp :: !p)); p)

let rec visited_already pos_list pos = match pos_list with
  | [] -> false
  | h::t -> if (h = pos) then true else (visited_already t pos)

let havent_visited_yet pos_list pos = not (visited_already pos_list pos)
					     
let remove_first_letter = (fun s -> String.sub s 1 ((String.length s) - 1))
			    
let rec string_to_list_of_chars = (fun s ->
    match s with
    | "" -> []
    | _ -> (String.get s 0) :: (string_to_list_of_chars (remove_first_letter s)))
					      
let rec join l1 l2 = match l1, l2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1,h2) :: join t1 t2

let view_to_tuples current_view =
  let l1 = string_to_list_of_chars (remove_first_letter current_view) in
  let l2 = ['U';'D';'L';'R'] in
  join l2 l1

let all_available_moves view_tuples current_pos pos_list =
  let rec aux vt cp pl list_of_moves_available n = match vt, !cp, n with
    | _, _, 0 -> list_of_moves_available
    |(_,'#')::t , (x,y), _ -> aux t cp pl list_of_moves_available (n-1)
    |('U',c)::t , (x,y), _ -> if (havent_visited_yet !pl (x,y+1)) then aux t cp pl ('U' :: list_of_moves_available) (n-1)
			      else aux t cp pl list_of_moves_available (n-1)
    |('D',c)::t , (x,y), _ -> if (havent_visited_yet !pl (x,y-1)) then aux t cp pl ('D' :: list_of_moves_available) (n-1)
			      else aux t cp pl list_of_moves_available (n-1)
    |('L',c)::t , (x,y), _ -> if (havent_visited_yet !pl (x-1,y)) then aux t cp pl ('L' :: list_of_moves_available) (n-1)
			      else aux t cp pl list_of_moves_available (n-1)
    |('R',c)::t , (x,y), _ -> if (havent_visited_yet !pl (x+1,y)) then aux t cp pl ('R' :: list_of_moves_available) (n-1)
			      else aux t cp pl list_of_moves_available (n-1)
  in aux view_tuples current_pos pos_list [] 4

let backtrack moves_made = match !moves_made with
  | [] -> 'K' (* end game and return character trophies *)
  | 'U' :: t -> moves_made := t; 'D'
  | 'D' :: t -> moves_made := t; 'U'
  | 'L' :: t -> moves_made := t; 'R'
  | 'R' :: t -> moves_made := t; 'L'

let make_move view_tuples current_pos pos_list =
  let moves_available = (all_available_moves view_tuples current_pos pos_list) in
  match moves_available with
  | [] -> backtrack moves_made_so_far
  | h :: t -> moves_made_so_far := h :: !moves_made_so_far; h
   
let rec contains list e = match list with
  | [] -> false
  | h :: t -> if h = e then true else contains t e
	       
let check_if_sitting_on_letter = (fun s -> match (String.get s 0) with
					   | '#' -> (letters_caught := !letters_caught)
					   | ' ' -> (letters_caught := !letters_caught)
					   | _ -> let letter = String.get s 0 in
						  let letter_and_location = (letter, !current_pos) in
						  if contains !letters_and_locations letter_and_location then
						    (letters_caught := !letters_caught)
						  else
						    (letters_and_locations := letter_and_location :: !letters_and_locations; 
						    (letters_caught := ((letter) :: !letters_caught))))

let rec get_kth_element_from_list list k = match k, list with
  | 1, h :: t -> h
  | _, h :: t-> get_kth_element_from_list t (k-1)

let rec list_of_chars_to_string clist =
  match clist with
  | [] -> ""
  | h :: t -> (String.make 1 (get_kth_element_from_list clist 1)) ^ (list_of_chars_to_string t);;

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail

let end_game () = "K" ^ (list_of_chars_to_string (sort(!letters_caught)))
  
let rec lets_play_harder current_pos pos_visited =
  let current_view = input_line stdin in
  check_if_sitting_on_letter current_view;
  let move = (make_move (view_to_tuples current_view) current_pos pos_visited) in
    if move = 'K' then print_endline (end_game ()) else
         (print_endline (Char.escaped move);
	 let cp = (update_current_pos current_pos move) in
	 let pv = (update_pos_visited pos_visited !cp) in
         lets_play_harder cp pv)

let start_game () = lets_play_harder current_pos pos_visited;;

start_game ()
