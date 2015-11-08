
let current_pos = ref (0,0)
let update_current_pos p = (fun c -> match c , !p with
				     | 'U' , (a,b) -> p := (a,b+1); p
				     | 'D' , (a,b) -> p := (a,b-1); p
				     | 'R' , (a,b) -> p := (a+1,b); p
				     | 'L' , (a,b) -> p := (a-1,b); p
			   )

let pos_visited = ref [(0,0)];;

let update_pos_visited p = (fun cp -> (p := (cp :: !p)); p);;

let rec visited_already pos_list pos = match pos_list with
  | [] -> false
  | h::t -> if (h = pos) then true else (we_visited_already t pos)

let havent_visited_yet pos_list pos = not (we_visited_already pos_list pos)
					     
let letters_caught = ref [] 

let remove_first_letter = (fun s -> String.sub s 1 ((String.length s) - 1))
			    
let rec string_to_list_of_chars = (fun s ->
    match s with
    | "" -> []
    | _ -> (String.get s 0) :: (string_to_list_of_chars (remove_first_letter s)))
					      
let rec join l1 l2 = match l1, l2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1,h2) :: join t1 t2;;

let view_to_tuples current_view =
  let l1 = string_to_list_of_chars (remove_first_letter current_view) in
  let l2 = ['U';'D';'L';'R'] in
  join l2 l1;;

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

let moves_made_so_far = ref [];;

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
  | h :: t -> moves_made_so_far := h :: !moves_made_so_far; h;;
   
(* 
(* method below moves to first new square available --> looks Up, then down, then left then right *)
let rec make_new_move view_tuples current_pos pos_list = match view_tuples, !current_pos with
  | [], _ -> 'K' (* no new move available *)
  | (_,'#') :: t , _ -> make_new_move t current_pos pos_list
  | ('U',char) :: t , (x,y) -> if (havent_visited_yet !pos_list (x,y+1)) then 'U'
			       else  make_new_move t current_pos pos_list
  | ('D',char) :: t , (x,y) -> if (havent_visited_yet !pos_list (x,y-1)) then 'D'
			       else  make_new_move t current_pos pos_list
  | ('L',char) :: t , (x,y) -> if (havent_visited_yet !pos_list (x-1,y)) then 'L'
			       else  make_new_move t current_pos pos_list
  | ('R',char) :: t , (x,y) -> if (havent_visited_yet !pos_list (x+1,y)) then 'R'
			       else  make_new_move t current_pos pos_list
 *)			       
let check_if_sitting_on_letter = (fun s -> match (String.get s 0) with
					   | '#' -> (letters_caught := !letters_caught)
					   | ' ' -> (letters_caught := !letters_caught)
					   | _ -> (letters_caught := ((String.get s 0) :: !letters_caught)))

