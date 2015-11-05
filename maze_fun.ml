(*  
read_line  (* reads from standard input *)
print_string  (* writes to standard output *)
print_newline (* writes to standard output *)
 *)

(* naive implementation -- always grabs first letter available (may grab same letter multiple times *)
let rec maze_robot = ()

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
    

let sort_char_list_alphabetically = ()	
let concatenate_list = ()
	
let rec lets_play n = match n with
  | 0 -> print_string "End of Game"
  | _ -> print_char (make_naive_move (read_line ())); print_newline(); (lets_play (n-1));;


let maze_3_4 = ['#';'#';'#';'#';
	        '#';'A';'B';'#';
	        '#';'#';'#';'#']
  
let rec get_kth_element_from_list list = (fun k -> match k, list with
  | 1, h :: t -> h
  | _, h :: t-> get_kth_element_from_list t (k-1))

let k_3_4 = get_kth_element_from_list maze_3_4
					  
let current_3_by_4_view_letters = ref ['A';'#';'#';'#';'B'] (* positions [6; 2; 10; 5; 7] in 3_by_4 maze *)

let current_3_by_4_view_positions = ref [6;2;10;5;7]
					  									  
let update_3_by_4_view_positions current_view_positions move = match move with
  | 'U' -> current_3_by_4_view_positions := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-4)]
					      
  | 'D' -> current_3_by_4_view_positions := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+4);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+4)]
  
  | 'L' -> current_3_by_4_view_positions := [((get_kth_element_from_list !current_3_by_4_view_positions 1)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)-1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)-1)]
 
  | 'R' -> current_3_by_4_view_positions := [((get_kth_element_from_list !current_3_by_4_view_positions 1)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 2)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 3)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 4)+1);
					     ((get_kth_element_from_list !current_3_by_4_view_positions 5)+1)]

let update_3_by_4_view_letters () = current_3_by_4_view_letters := [k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 1);
				           			    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 2);
					         		    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 3);
						        	    k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 4);
							            k_3_4 (get_kth_element_from_list !current_3_by_4_view_positions 5);
							           ]
let rec list_of_chars_to_string clist =
  match clist with
  | [] -> ""
  | h :: t -> (String.make 1 (get_kth_element_from_list clist 1)) ^ (list_of_chars_to_string t)
 
  let rec string_to_list_of_chars = (fun s ->
    match s with
    | "" -> []
    | _ -> (String.get s 0) :: (string_to_list_of_chars (remove_first_letter s)))

