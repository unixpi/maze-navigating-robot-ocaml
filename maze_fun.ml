(*  
read_line  (* reads from standard input *)
print_string  (* writes to standard output *)
print_newline (* writes to standard output *)
 *)

(* naive implementation -- always grabs first letter available (may grab same letter multiple times *)
let rec maze_robot = ()


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

let rec there_is_a_letter_we_can_move_to  = (fun s -> match (string_to_list_of_chars (remove_first_letter s)) with
  | [] -> false
  | h :: t -> if (h = '#' || h = ' ') then (there_is_a_letter_we_can_move_to (remove_first_letter s))
	      else
		true)
					      
let rec join l1 l2 = match l1, l2 with
  | [], [] -> []
  | h1::t1, h2::t2 -> (h1,h2) :: join t1 t2;;
  
let view_to_tuples current_view =
  let l1 = string_to_list_of_chars (remove_first_letter current_view) in
  let l2 = ['U';'D';'L';'R'] in
  join l1 l2;;

(* method below moves to first new square available --> looks Up, then down, then left then right *)
let rec make_new_move view_tuples current_pos pos_list = match view_tuples, current_pos with
  | [], _ -> 'K' (* no new move available *)
  | (_,'#') :: t , _ -> make_new_move t current_pos pos_list
  | ('U',_) :: t , (x,y) -> if (havent_visited_yet !pos_list (x,y+1)) then 'U' else  make_new_move t current_pos pos_list
  | ('D',_) :: t , (x,y) -> if (havent_visited_yet !pos_list (x,y-1)) then 'D' else  make_new_move t current_pos pos_list
  | ('L',_) :: t , (x,y) -> if (havent_visited_yet !pos_list (x-1,y)) then 'L' else  make_new_move t current_pos pos_list
  | ('R',_) :: t , (x,y) -> if (havent_visited_yet !pos_list (x+1,y)) then 'R' else  make_new_move t current_pos pos_list
  
			       
let check_if_sitting_on_letter = (fun s -> match (String.get s 0) with
					   | '#' -> (letters_caught := !letters_caught)
					   | ' ' -> (letters_caught := !letters_caught)
					   | _ -> (letters_caught := ((String.get s 0) :: !letters_caught)))

			
let move_to_first_available_letter = (fun s ->
  let rec aux s moves_left =  match (string_to_list_of_chars s), moves_left with
    | h1 :: t1 , h2 :: t2 -> if (h1 = '#' || h1 = ' ') then (aux (remove_first_letter s) t2)
			     else 
			        h2 
  in aux (remove_first_letter s) ['U';'D';'L';'R'] )

let move_to_first_available_space = (fun s ->
  let rec aux s moves_left =  match (string_to_list_of_chars s), moves_left with
    | h1 :: t1 , h2 :: t2 -> if (h1 = '#') then (aux (remove_first_letter s) t2)
			     else 
			       h2
   in aux (remove_first_letter s) ['U';'D';'L';'R'] ) 
				   
let make_naive_move = (fun s -> (check_if_sitting_on_letter s); if (there_is_a_letter_we_can_move_to s) then (move_to_first_available_letter s)
				else
				  (move_to_first_available_space s))
let make_slightly_better_move current_pos current_view pos_list =
  check_if_sitting_on_letter current_view;
  if (there_is_a_new_letter current_view current_pos pos_list) then (move_to_first_available_letter s)
  else
    move_to_first_available_space current_view;;
(*
let make_even_better_move current_pos current_view pos_list =
  check_if_sitting_on_letter current_view;
  move_to_all_new_locations
 *)

let sort_char_list_alphabetically = ()	
let concatenate_list = ()

(* testing  *)
  
let maze_3_4 = ['#';'#';'#';'#';
	        '#';'A';'B';'#';
	        '#';'#';'#';'#']
  
let rec get_kth_element_from_list list = (fun k -> match k, list with
  | 1, h :: t -> h
  | _, h :: t-> get_kth_element_from_list t (k-1))

let k_3_4 = get_kth_element_from_list maze_3_4
					  
let current_3_by_4_view_letters = ref ['A';'#';'#';'#';'B'] (* positions [6; 2; 10; 5; 7] in 3_by_4 maze *)

let current_3_by_4_view_positions = ref [6;2;10;5;7]
					  									  
let update_3_by_4_view_positions c move = match move with
  | 'U' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-4)]
					      
  | 'D' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+4)]
  
  | 'L' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-1)]
 
  | 'R' -> c := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+1)]

let update_3_by_4_view_letters  = (fun c ->
    c := [k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 1);
				           			    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 2);
					         		    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 3);
						        	    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 4);
							            k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 5);
							           ])
let rec list_of_chars_to_string clist =
  match clist with
  | [] -> ""
  | h :: t -> (String.make 1 (get_kth_element_from_list clist 1)) ^ (list_of_chars_to_string t);;


								      
let update_view = (fun m -> (update_3_by_4_view_positions current_3_by_4_view_positions m);
			     update_3_by_4_view_letters current_3_by_4_view_letters; list_of_chars_to_string !current_3_by_4_view_letters)

let starting_view = "A###B"
let current_view  = list_of_chars_to_string !current_3_by_4_view_letters;;								


(* let lets_play current_view =
  let line = input_line stdin in
    print_char (make_naive_move line);;
 *)
		   
let rec lets_play n current_view current_pos pos_visited = match n with
  | 0 -> "End of Game"
  | _ -> let move = (make_naive_move current_view) in
         Printf.printf "%s\n%!" current_view;
         Printf.printf "%c\n%!" move;
	 let cp = (update_current_pos current_pos move) in
	 let pv = (update_pos_visited pos_visited !cp) in
         lets_play (n-1) (update_view move) cp pv

(* start_game allows us to play n turns on the simplest 3x4 maze, one move is all that is needed to win although our robot is
unaware of this right now.. *)  
let start_game = (fun n -> lets_play n starting_view current_pos pos_visited) 

  (* to do
        1. implement general solution algorithm
        2. create function to sort and return character trophies
   *)
(* to reset *)		   
(current_pos := (0,0); pos_visited := [(0,0)];
current_3_by_4_view_positions := [6;2;10;5;7];
current_3_by_4_view_letters := ['A';'#';'#';'#';'B'])
